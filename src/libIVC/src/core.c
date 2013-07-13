// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
//
#include <xenctrl.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>
#include <libIVC.h>
#include "ivc_private.h"
#include <stdio.h>
#include <arpa/inet.h>
#include <xenstore.h>
#include <errno.h>

#define PROT_READWRITE (PROT_READ | PROT_WRITE)

struct xs_handle *xsd = 0;
xc_evtchn *xce = NULL;
xc_gnttab *xcg = NULL;

extern int asprintf (char **__restrict __ptr,
                     __const char *__restrict __fmt, ...);

void initialize_libIVC_library(void)
{
  xsd = xs_domain_open();
  xcg = xc_gnttab_open(NULL, XC_OPENFLAG_NON_REENTRANT);
}

// Count the number of grant references that are likely to be present in a
// string.
static int num_grefs(const char *buf)
{
  int count = 1;

  if(!buf) {
    return 0;
  }

  if(strlen(buf) < 2) {
    return 0;
  }

  while((buf = strchr(buf, ','))) {
    ++buf;
    ++count;
  }

  return count;
}

// Parse out a list of grant references from a string.  This will allocate, so
// make sure to free what you get back, if it's non-null.
static unsigned int *parse_grefs(char *buf, int *grefs_len)
{
  unsigned int *grefs = NULL;
  char *pos  = buf;
  char *tok  = NULL;
  char *save = NULL;
  int   len  = 0;
  int   i    = 0;

  if(!buf || !grefs_len || *pos != '[') {
    return NULL;
  }

  ++pos;
  len        = num_grefs(pos);
  *grefs_len = len;

  if(len == 0) {
    return NULL;
  }

  grefs = (unsigned int*)malloc(len*sizeof(unsigned int));
  if(!grefs) {
    printf("Unable to malloc enough space for grant refs\n");
    return NULL;
  }

  for(i=0; i<len; ++i, pos = NULL) {
    tok = strtok_r(pos, ",", &save);
    if(tok == NULL) {
      break;
    }
    sscanf(tok, "%*s (GrantRef %i)", grefs+i);
  }

  return grefs;
}


/*
 * @brief Bind the memory, and event channel to start IVC between two domains
 *
 * NOTE:  this allocates memory in the channel_core struct, and as we don't have
 * a way to cleanup these structures, this memory will live for the lifetime
 * of the program.
 *
 * @param name     the channel name
 * @param otherDom out parameter for the domain of the other side
 * @param port     where to place the bound event channel
 * @param chan     wrapper for the memory associated with the channel
 *
 * @return 0 on failure, and 1 on success.
 *
 */
int bind_memory_and_port(char *name, unsigned long *otherDom,
                         struct channel_core *chan)
{
  char *grefStr = NULL, *echanStr = NULL, *myDomStr = NULL, *otherDomStr = NULL;
  unsigned int echan = 0, myDom = 0, len = 0;
  bool done = 0;
  char *key = NULL;
  unsigned int *grefs = NULL;
  int  grefs_len = 0;

  chan->xce = xc_evtchn_open(NULL, XC_OPENFLAG_NON_REENTRANT);
  if((long)chan->xce == -1) {
    fprintf(stderr, "Failed to open evtchn handle\n");
    return 0;
  }

  // Pull our domain id out of the xenstore.
  while(!myDomStr) {
    xs_transaction_t tran;
    int len = 0;

    tran = xs_transaction_start(xsd);
    myDomStr = xs_read(xsd, tran, "domid", (unsigned int *)&len);
    xs_transaction_end(xsd, tran, 0);
  }
  sscanf(myDomStr, "%i", &myDom);

  // Do the initial set up. 
  while(!done) {
    xs_transaction_t tran;
    unsigned int num_dir_ents = 0;
    char **halvmdir = NULL;
    char *val = NULL;

    tran = xs_transaction_start(xsd);
    halvmdir = xs_directory(xsd, tran, "/halvm", &num_dir_ents);
    if(!halvmdir) // A nil halvmdir is how libxenstore notes nonextant dirs.
      return 0;

    // Recreate the directory, if it's not there
    free(key), asprintf(&key, "/halvm/%s", name);
    xs_mkdir(xsd, tran, key);

    // Throw in our domain identifier.
    free(key), asprintf(&key, "/halvm/%s/starterDomId", name);
    free(val), asprintf(&val, "DomId %i", myDom);
    xs_write(xsd, tran, key, val, strlen(val));

    // End this transaction, just for fun.
    done = xs_transaction_end(xsd, tran, 0);
  }

  // Spin until we've pulled all the needed keys.
  free(key), asprintf(&key, "/halvm/%s/grant-refs", name);
  while(!grefStr) { grefStr = xs_read(xsd, 0, key, &len); }
  free(key), asprintf(&key, "/halvm/%s/accepterDomId", name);
  while(!otherDomStr) { otherDomStr = xs_read(xsd, 0, key, &len);  }
  free(key), asprintf(&key, "/halvm/%s/event-channel", name);
  while(!echanStr) { echanStr = xs_read(xsd, 0, key, &len);  }

  // Remove the directory, now that the connection is made.
  free(key), asprintf(&key, "/halvm/%s", name);
  xs_rm(xsd, 0, key);

  // Translate these into useful bits of information.
  grefs = parse_grefs(grefStr, &grefs_len);
  if(grefs == NULL) {
    printf("Unable to parse grant refs\n");
    return 0;
  }

  sscanf(echanStr, "Port %i", &echan);
  sscanf(otherDomStr, "DomId %li", otherDom);

  // map the grant refs
  chan->mem = xc_gnttab_map_domain_grant_refs(xcg, grefs_len, *otherDom,
      grefs, PROT_READWRITE);
  if(chan->mem == NULL) {
    perror("Couldn't map grant reference!");
    return 0;
  }

  free(grefs);

  // grant the event channel
  chan->port = xc_evtchn_bind_interdomain(chan->xce, *otherDom, echan);
  if(chan->port < 0) {
    printf("Couldn't bind event channel!\n");
    return 0;
  }

  chan->ring_size = (grefs_len * 4096) - sizeof(ivc_shared_page);
  chan->block     = (ivc_shared_page*)(chan->mem + chan->ring_size);

  return 1;
}

int resize_channel_core(struct channel_core *chan, unsigned int new, char **mem)
{
  // size, including the overhead space
  int size = chan->ring_size + sizeof(ivc_shared_page);

  if(new > chan->ring_size) {
    printf("Unable to resize channel to more memory than it already has\n");
    return 0;
  }

  // calculate the address of the new, free memory
  *mem = chan->mem + new;

  // resize the channel_core
  // NOTE: this sets produced and consumed to 0.  I don't think that there's
  // a good way to preserve these values, as they sort of end up depending on
  // the amount of memory that's allocated to mean what they mean.
  chan->ring_size             = new - sizeof(ivc_shared_page);
  chan->block                 = (ivc_shared_page*)(chan->mem + chan->ring_size);
  chan->block->bytes_consumed = 0;
  chan->block->bytes_produced = 0;

  return (size - new);
}

uint32_t pull_next_size(struct channel_core *chan)
{
  uint32_t size = 0;

  internal_read(chan, (void *)&size, sizeof(size));
  size = ntohl(size);

  printf("pull_next_size(%p) = 0x%x\n", chan, size);
  return size;
}

void push_next_size(struct channel_core *chan, uint32_t size)
{
  printf("push_next_size(%p, 0x%x)\n", chan, size);
  size = htonl(size);
  internal_write(chan, &size, sizeof(size));
}

#define RING_DATA_SIZE(x) x->ring_size

#define CHAN_FREE_SPACE(x) (x->ring_size - chan_free_write_space(x) - 1)

static inline unsigned long chan_free_write_space(unsigned long ring_size,
    unsigned long prod, unsigned long cons)
{
  // overhad is included in ring_size
  if(prod >= cons) {
    return (ring_size - (prod - cons) - 1);
  } else {
    // wraparound case
    return (cons - prod - 1);
  }
}

static unsigned long chan_free_read_space(unsigned long ring_size,
    unsigned long prod, unsigned long cons)
{
  return (ring_size - chan_free_write_space(ring_size, prod, cons) - 1);
}

static inline unsigned int wait_chan(struct channel_core *chan)
{
  int port;
  
  port = xc_evtchn_pending(chan->xce);
  if(port >= 0) {
    xc_evtchn_unmask(chan->xce, port);
  }

  return port == chan->port;
}

void internal_read(struct channel_core *chan, void *buffer, int size)
{
  unsigned long readable_space;
  unsigned long prod;
  unsigned long cons;
  unsigned long buflen = chan->ring_size;
  unsigned long read_amt;

  while(size > 0) {
    readable_space = 0, read_amt = 0;
    void *start_cpy;

    // Wait for available data.
    while(1) {
      prod           = chan->block->bytes_produced;
      cons           = chan->block->bytes_consumed;
      readable_space = chan_free_read_space(buflen, prod, cons);

      if(readable_space > 0) {
        break;
      }

      wait_chan(chan);
    }


    // determine how much space can be read
    read_amt = (readable_space > size) ? size : readable_space;

    // Copy the data to the buffer
    start_cpy = chan->mem + cons;

    rmb();
    if(cons + read_amt > buflen) {
      int first  = buflen - cons;
      int second = read_amt - first;
      memcpy(buffer,       start_cpy, first);
      memcpy(buffer+first, chan->mem, second);
      mb();
      chan->block->bytes_consumed = second;
    } else {
      memcpy(buffer, start_cpy, read_amt);
      mb();
      chan->block->bytes_consumed += read_amt;
    }

    // Update the various counters
    size   -= read_amt;
    buffer += read_amt;


    // notify that we are trying to read something
    xc_evtchn_notify(chan->xce, chan->port);
  }
}

void internal_write(struct channel_core *chan, void *buffer, int size)
{
  int free_space;
  unsigned long prod;
  unsigned long cons;
  unsigned long buflen = chan->ring_size;

  while(size > 0) {
    void *start_cpy;
    int write_amt = 0;

    // Wait for space to write.
    while(1) {
      prod       = chan->block->bytes_produced;
      cons       = chan->block->bytes_consumed;
      free_space = chan_free_write_space(buflen, prod, cons);

      if(free_space > 0) {
        break;
      }

      wait_chan(chan);
    }

    // determine how much free space we have for writing
    write_amt = (free_space > size) ? size : free_space;

    // Copy the data to the buffer
    start_cpy = chan->mem + prod;

    rmb();
    if(prod + write_amt > buflen) {
      // we are wrapping around the end of the buffer
      int first  = buflen - prod;
      int second = write_amt - first;
      memcpy(start_cpy, buffer,       first);
      memcpy(chan->mem, buffer+first, second);
      wmb();
      // reset the number of produced bytes
      chan->block->bytes_produced = second;
    } else {
      memcpy(start_cpy, buffer, write_amt);
      wmb();
      chan->block->bytes_produced += write_amt;
    }

    // Update the various counters
    size   -= write_amt;
    buffer += write_amt;


    // notify that we would like to read something
    xc_evtchn_notify(chan->xce, chan->port);
  }
}

