// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
//
#include "libIVC.h"
#include "ivc_private.h"
#include <xenctrl.h>
#include <xs.h>
#include <xen/io/xs_wire.h>
#include <string.h>
#include <stdio.h>
#include <sys/mman.h>
#include <stdlib.h>

struct xbackend {
  // Turns out we don't need anything here.
};

#define PROT_READWRITE (PROT_READ | PROT_WRITE)

extern int asprintf (char **__restrict __ptr,
                     __const char *__restrict __fmt, ...);

static void mfree(void *ptr)
{
  if(ptr) free(ptr);
}

xen_backend *create_responder(char *name)
{
  unsigned int len = 0, myDom = 0;
  char *myDomStr = NULL;
  bool done = 0;
  xen_backend *res = malloc(sizeof(xen_backend));

  // Pull our domain id out of the XenStore. 
  while(!myDomStr) myDomStr = xs_read(xsd, 0, "domid", (unsigned int*)&len);
  sscanf(myDomStr, "%i", &myDom);
  // Do the initial set up.
  while(!done) {
    unsigned int num_dir_ents = 0;
    char **halvmdir = NULL;
    char *key = NULL, *val = NULL;

    // nil halvmdir == nonextant directory
    halvmdir = xs_directory(xsd, 0, "/halvm", &num_dir_ents);
    if(!halvmdir)
      return 0;

    // Remove the old directory if it exists.
    asprintf(&key, "/halvm/%s", name);
    xs_rm(xsd, 0, key);

    // Recreate the directory.
    xs_mkdir(xsd, 0, key);

    // Throw in our domain identifier and create the subdirectory.
    mfree(key); asprintf(&key, "/halvm/%s/server-id", name);
    mfree(val); asprintf(&val, "DomId %i", myDom);

    xs_write(xsd, 0, key, val, strlen(val));
    mfree(val);

    mfree(key); asprintf(&key, "/halvm/%s/clients", name);
    xs_mkdir(xsd, 0, key);

    // Set the watch
    done = xs_watch(xsd, key, "cwatch");
    mfree(key);
  }

  bzero(res, sizeof(res));
  return res;  
}

void accept_connection(xen_backend *be, unsigned long *otherDom,
                       void **ptr, void *data)
{
  evtchn_port_t *port = (evtchn_port_t*)data;
  *ptr = NULL;

  while(!*ptr) {
    unsigned int num_strs = 0;
    char **mods = xs_read_watch(xsd, &num_strs);

    *ptr = NULL; *otherDom = 0; *port = 0;
    if(!strcmp(mods[XS_WATCH_TOKEN], "cwatch")) { 
      int len = strlen(mods[XS_WATCH_PATH]);

      if((len > 6) && !strcmp(mods[XS_WATCH_PATH] + (len - 6), "/state")) {
        char *val;

        val = xs_read(xsd, 0, mods[XS_WATCH_PATH], &num_strs);
        if(!strcmp(val, "Sort_of_goodish")) {
          char *key = NULL, *odomStr = NULL, *grefStr = NULL, *chanStr = NULL;
          char *basedir = strdup(mods[XS_WATCH_PATH]);
          unsigned int gref = 0, echan = 0;
         
          basedir[len - 6] = 0;
          mfree(key), asprintf(&key, "%s/domain-id", basedir);
          while(!odomStr) odomStr = xs_read(xsd, 0, key, &num_strs);
          mfree(key), asprintf(&key, "%s/grant-ref0", basedir);
          while(!grefStr) grefStr = xs_read(xsd, 0, key, &num_strs);
          mfree(key), asprintf(&key, "%s/event-channel0", basedir);
          while(!chanStr) chanStr = xs_read(xsd, 0, key, &num_strs);
          //
          sscanf(grefStr, "%*s (GrantRef %i)", &gref);
          sscanf(chanStr, "Port %i", &echan);
          sscanf(odomStr, "DomId %li", otherDom);
          //
          *port = xc_evtchn_bind_interdomain(xce, *otherDom, echan);
          if((int)*port < 0) {
            printf("Couldn't bind event channel\n");
          } else {
            *ptr = xc_gnttab_map_grant_ref(xcg, *otherDom, gref,PROT_READWRITE);
            if(!*ptr) printf("Couldn't map grant reference!\n");
          }
        }
      } 
    }
  }
}
