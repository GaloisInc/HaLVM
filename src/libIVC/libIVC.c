#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <libIVC.h>
#include <assert.h>
#include <xenctrl.h>
#include <xenstore.h>

#define PROT_READWRITE (PROT_READ | PROT_WRITE)

/*****************************************************************************/
/** Opening / Closing High Level Interfaces **********************************/
/*****************************************************************************/

struct libIVC_interface {
  xc_interface     *xc;
  struct xs_handle *xs;
  xc_evtchn        *ec;
  xc_gnttab        *gt;
  xc_gntshr        *gs;
};

libIVC_t *openIVCLibrary()
{
  libIVC_t *retval;

  assert( retval = calloc(1, sizeof(struct libIVC_interface)) );
  assert( retval->xc = xc_interface_open(NULL, NULL, 0) );
  assert( retval->xs = xs_open(0) );
  assert( retval->ec = xc_evtchn_open(NULL, 0) );
  assert( retval->gt = xc_gnttab_open(NULL, 0) );
  retval->gs = xc_gntshr_open(NULL, 0); /* may not be available on all plats */

  return retval;
}

void closeIVCLibrary(libIVC_t *iface)
{
  xc_evtchn_close(iface->ec);
  xs_close(iface->xs);
  xc_interface_close(iface->xc);
  xc_gnttab_close(iface->gt);
  if(iface->gs) xc_gntshr_close(iface->gs);
  free(iface);
}

/*****************************************************************************/
/** Channel Setup ************************************************************/
/*****************************************************************************/

static uint32_t getMyDomId(libIVC_t *);
static uint32_t *parseRefs(char *str, uint32_t *len);
static char     *showGrantRefList(uint32_t *grants, uint32_t num);
static ivc_connection_t *buildChannel(libIVC_t*, uint32_t, ivc_contype_t,
                                      evtchn_port_t, void *, uint32_t, float);
static uint32_t computeModulus(uint32_t);

struct bufstate
{
  volatile uint32_t consumed;
  volatile uint32_t produced;
};

struct ivc_connection
{
  libIVC_t *iface;
  uint32_t peer;
  ivc_contype_t type;
  evtchn_port_t port;
  uint32_t insize, outsize;
  uint32_t inmod, outmod;
  void *inbuf, *outbuf;
  struct bufstate *input;
  struct bufstate *output;
};

ivc_connection_t *makeConnection(libIVC_t *iface,
                                 char *name,
                                 ivc_contype_t type,
                                 float perc)
{
  char *key, *val, *rdomstr = NULL, *rrefstr = NULL, *rpstr = NULL;
  uint32_t me, other, num_refs, *grants;
  struct xs_permissions perms;
  ivc_connection_t *res;
  evtchn_port_t port;
  unsigned int len;
  void *buffer;

  /* me <- xsGetDomId */
  me = getMyDomId(iface);
  /* removePath xs targetPath */
  asprintf(&key, "/rendezvous/%s", name);
  xs_rm(iface->xs, 0, key);
  /* xsMakeDirectory xs targetPath */
  xs_mkdir(iface->xs, 0, key);
  /* xsSetPermissions xs targetPath [ReadWritePerm me] */
  perms.id = me;
  perms.perms = XS_PERM_READ | XS_PERM_WRITE;
  xs_set_permissions(iface->xs, 0, key, &perms, 1);
  /* xsWrite xs (targetPath ++ "/LeftDomId") (show me) */
  free(key), asprintf(&key, "/rendezvous/%s/LeftDomId", name);
             asprintf(&val, "dom%d", me);
  xs_write(iface->xs, 0, key, val, strlen(val));
  /* other <- read <$> waitForKey xs (targetPAth ++ "/RightDomId") */
  free(key), asprintf(&key, "/rendezvous/%s/RightDomId", name);
  while(!rdomstr) { rdomstr = xs_read(iface->xs, 0, key, &len); };
  sscanf(rdomstr, "dom%d", &other);
  /* grants <- read <$> waitForKey xs (targetPAth ++ "/RightGrantRefs") */
  free(key), asprintf(&key, "/rendezvous/%s/RightGrantRefs", name);
  while(!rrefstr) { rrefstr = xs_read(iface->xs, 0, key, &len); }
  grants = parseRefs(rrefstr, &num_refs);
  buffer = xc_gnttab_map_domain_grant_refs(iface->gt, num_refs, other, grants,
                                           PROT_READWRITE);
  assert(buffer);
  /* ports  <- read <$> waitForKey xs (targetPAth ++ "/RightPorts") */
  free(key), asprintf(&key, "/rendezvous/%s/RightPorts", name);
  while(!rpstr) { rpstr = xs_read(iface->xs, 0, key, &len); }
  sscanf(rpstr, "[echan:%d]", &port);
  port = xc_evtchn_bind_interdomain(iface->ec, other, port);
  assert(port >= 0);
  /* res <- acceptConnection other grants ports extra */
  res = buildChannel(iface, other, type, port, buffer, num_refs * 4096, perc);
  /* xsWrite xs (targetPath ++ "/LeftConnectionConfirmed") "True" */
  free(key), asprintf(&key, "/rendezvous/%s/LeftConnectionConfirmed", name);
  free(val), asprintf(&val, "True");
  xs_write(iface->xs, 0, key, val, strlen(val));
  /* return res */
  return res;
}

ivc_connection_t *acceptConnection(libIVC_t *iface,
                                   char *name,
                                   ivc_contype_t type,
                                   uint32_t num_pages,
                                   float perc)
{
  char *key, *val, *domStr = NULL;
  uint32_t me, other, *gs, ps;
  unsigned int len;
  void *buffer;

  /* we can only do this if we create grant references */
  if(!iface->gs) return NULL;
  /* other <- read `fmap` waitForKey xs (targetPath ++ "/LeftDomId) */
  asprintf(&key, "/rendezvous/%s/LeftDomId", name);
  while(!domStr) { domStr = xs_read(iface->xs, 0, key, &len); }
  sscanf(domStr, "dom%d", &other);
  /* me <- xsGetDomId */
  me = getMyDomId(iface);
  /* (gs, ps, confirm) <- makeConnection other extra */
  gs = calloc(num_pages, sizeof(uint32_t));
  buffer = xc_gntshr_share_pages(iface->gs, other, num_pages, gs, 1);
  assert(buffer);
  ps = xc_evtchn_bind_unbound_port(iface->ec, other);
  assert(ps);
  /* xsWrite xs (targetPath ++ "/RightDomId") (show me) */
  free(key), asprintf(&key, "/rendezvous/%s/RightDomId", name);
  free(val), asprintf(&val, "dom%d", me);
  xs_write(iface->xs, 0, key, val, strlen(val));
  /* xsWrite xs (targetPath ++ "/RightGrantRefs") (show gs) */
  free(key), asprintf(&key, "/rendezvous/%s/RightGrantRefs", name);
  free(val), val = showGrantRefList(gs, num_pages);
  xs_write(iface->xs, 0, key, val, strlen(val));
  /* xsWrite xs (targetPath ++ "/RightPorts") (show ps) */
  free(key), asprintf(&key, "/rendezvous/%s/RightPorts", name);
  free(val), asprintf(&val, "[echan:%d]", ps);
  xs_write(iface->xs, 0, key, val, strlen(val));
  /* _ <- waitForKey xs (targetPath ++ "/LeftConnectionConfirmed") */
  free(key), asprintf(&key, "/rendezvous/%s/LeftConnectionConfirmed", name);
  free(val), val = NULL;
  while(!val) { val = xs_read(iface->xs, 0, key, &len); }
  /* removePath xs targetPath */
  free(key), asprintf(&key, "/rendezvous/%s", name);
  xs_rm(iface->xs, 0, key);
  /* confirm */
  return buildChannel(iface, other, type, ps, buffer, 4096 * num_pages, perc);
}

void closeConnection(libIVC_t *iface, ivc_connection_t *con)
{
  void *ptr = con->inbuf ? con->inbuf : con->outbuf;
  uint32_t size;

  switch(con->type) {
    case ivcInputChannel:
      size = con->insize + sizeof(struct bufstate);
      break;
    case ivcOutputChannel:
      size = con->outsize + sizeof(struct bufstate);
      break;
    case ivcInputOutputChannel:
      size = con->insize + con->outsize + (2 * sizeof(struct bufstate));
      break;
    default:
      assert(0);
  }

  xc_gnttab_munmap(iface->gt, ptr, (size + 4095) / 4096);
  free(con);
}

static uint32_t getMyDomId(libIVC_t *iface)
{
  unsigned int len = 0;
  uint32_t retval;
  char *myDomStr;

  myDomStr = xs_read(iface->xs, 0, "domid", &len);
  if(!myDomStr)
    return 0;

  assert( sscanf(myDomStr, "%i", &retval) == 1 );
  return retval;
}

static uint32_t *parseRefs(char *str, uint32_t *count)
{
  uint32_t *retval, index, i;

  for(i = 0, *count = 0; str[i]; i++)
    if(str[i] == ':')
      *count += 1;
  assert(count > 0);

  retval = calloc(*count, sizeof(uint32_t));
  assert(retval);

  for(i = 0, index = 0; i < *count; i++) {
    int res;

    while(str[index] != ':') index += 1;
    res = sscanf(&(str[index]), ":%d", &(retval[i]));
    assert(res == 1);
    index += 1;
  }

  return retval;
}

static char *showGrantRefList(uint32_t *grants, uint32_t num)
{
  char **eachref = calloc(num, sizeof(char *));
  uint32_t i, idx, total_len;
  char *retval;

  for(i = 0, total_len = 0; i < num; i++) {
    asprintf(&(eachref[i]), "grant:%d", grants[i]);
    total_len += strlen(eachref[i]);
  }

  total_len += 2 /* braces */ + (num - 1) /* commas */ + 1 /* null */;
  retval = calloc(total_len, sizeof(char));

  retval[0]           = '[';
  retval[total_len-2] = ']';
  retval[total_len-1] = 0;

  for(i = 0, idx = 1; i < num; i++) {
    size_t size = strlen(eachref[i]);
    memcpy(&(retval[idx]), eachref[i], size);
    idx += size;
    if(i != (num - 1)) retval[idx++] = ',';
    free(eachref[i]);
  }
  free(eachref);

  return retval;
}

static ivc_connection_t *buildChannel(libIVC_t *iface,
                                      uint32_t peer,
                                      ivc_contype_t type,
                                      evtchn_port_t port,
                                      void *buffer,
                                      uint32_t buffer_size,
                                      float perc)
{
  ivc_connection_t *res = calloc(1, sizeof(ivc_connection_t));

  res->iface = iface;
  res->peer = peer;
  res->type = type;
  res->port = port;

  switch(type) {
    case ivcInputChannel:
      res->insize = buffer_size - sizeof(struct bufstate);
      res->outsize = 0;
      res->inmod = computeModulus(res->insize);
      res->outmod = 0;
      res->inbuf = buffer;
      res->outbuf = NULL;
      res->input  = (struct bufstate *)((uintptr_t)buffer + res->insize);
      res->output = NULL;
      break;
    case ivcOutputChannel:
      res->insize = 0;
      res->outsize = buffer_size - sizeof(struct bufstate);
      res->inmod = 0;
      res->outmod = computeModulus(res->outsize);
      res->inbuf = NULL;
      res->outbuf = buffer;
      res->input = NULL;
      res->output = (struct bufstate *)((uintptr_t)buffer + res->outsize);
      break;
    case ivcInputOutputChannel:
      /* base computation */
      res->insize = (uint32_t)(perc * (float)buffer_size);
      res->outsize = buffer_size - res->insize;
      res->inbuf = buffer;
      res->outbuf = (void*)((uintptr_t)buffer + res->insize);
      /* real computation */
      res->insize -= sizeof(struct bufstate);
      res->outsize -= sizeof(struct bufstate);
      res->inmod = computeModulus(res->insize);
      res->outmod = computeModulus(res->outsize);
      res->input = (struct bufstate *)((uintptr_t)res->inbuf + res->insize);
      res->output = (struct bufstate *)((uintptr_t)res->outbuf + res->outsize);
      break;
    default:
      assert(0);
  }

  return res;
}

static uint32_t computeModulus(uint32_t size)
{
  uint64_t size64 = size;
  uint64_t q = 0x100000000ULL / size64;
  uint32_t base = (uint32_t)(q * size64);

  return (base == 0) ? (uint32_t)(q * (size64 - 1)) : base;
}

/*****************************************************************************/
/** Reading and writing from channels ****************************************/
/*****************************************************************************/

static uint32_t getAvail(uint32_t, struct bufstate *);
static void     readRaw(ivc_connection_t *, void *, uint32_t);
static void     writeRaw(ivc_connection_t *, void *, uint32_t);

void *getData(ivc_connection_t *con)
{
  void *item_start, *item_cur, *item_end;
  uint32_t avail, rd_amount;
  unsigned long item_size;

  assert( con->inbuf );

  /* wait until we have enough data to read the size */
  do { avail = getAvail(con->inmod, con->input); }
    while(avail < sizeof(unsigned long));

  /* read the size, allocate the buffer, compute various pointers */
  readRaw(con, &item_size, sizeof(unsigned long));
  item_start = item_cur = malloc(item_size);
  item_end = (void*)((uintptr_t)item_start + item_size);

  /* read in the data */
  while(item_cur < item_end) {
    avail = getAvail(con->inmod, con->input);
    rd_amount = (avail > item_size) ? item_size : avail;
    if(rd_amount > 0) {
      readRaw(con, item_cur, rd_amount);
      item_cur = (void*)((uintptr_t)item_cur + rd_amount);
      item_size -= rd_amount;
    } else usleep(0);
  }
  assert(item_size == 0);

  return item_start;
}

void putData(ivc_connection_t *con, void *buffer, size_t size)
{
  void *buf_cur = buffer, *buf_end = (void*)((uintptr_t)buffer + size);
  unsigned long item_size = size;
  uint32_t avail, wrt_amount;

  assert( con->outbuf );

  /* wait until we have enough data to write the size, then do so */
  do { avail = con->outsize - getAvail(con->outmod, con->output); }
    while(avail < sizeof(unsigned long));
  writeRaw(con, &item_size, sizeof(unsigned long));

  /* write out the data */
  while(buf_cur < buf_end) {
    avail = con->outsize - getAvail(con->outmod, con->output);
    wrt_amount = (avail > item_size) ? item_size : avail;
    if(wrt_amount > 0) {
      writeRaw(con, buf_cur, wrt_amount);
      buf_cur = (void*)((uintptr_t)buf_cur + wrt_amount);
      item_size -= wrt_amount;
    } else usleep(0);
  }
  assert(item_size == 0);
}

uint32_t connectionPeer(ivc_connection_t *con)
{
  return con->peer;
}

static uint32_t getAvail(uint32_t mod, struct bufstate *state)
{
  uint32_t prod = state->produced;
  uint32_t cons = state->consumed;
  return (prod >= cons) ? (prod - cons) : (prod + (mod - cons));
}

static void readRaw(ivc_connection_t *con, void *buffer, uint32_t size)
{
  uint32_t off = con->input->consumed % con->insize;
  void *basep = (void*)((uintptr_t)con->inbuf + off);

  if(off + size > con->insize) {
    /* we need to split this up, as we're wrapping around */
    uint32_t part1sz = con->insize - off;
    memcpy(basep, buffer, part1sz);
    memcpy(con->inbuf, (void*)((uintptr_t)buffer + part1sz), size - part1sz);
  } else {
    /* we can just write it in directly here */
    memcpy(basep, buffer, size);
  }
  con->input->consumed = (con->input->consumed + size) % con->inmod;
  __sync_synchronize();
  xc_evtchn_notify(con->iface->ec, con->port);
}

static void writeRaw(ivc_connection_t *con, void *buffer, uint32_t size)
{
  uint32_t off = con->output->produced % con->outsize;
  void *basep = (void*)((uintptr_t)con->outbuf + off);

  if(off + size > con->outsize) {
    uint32_t part1sz = con->outsize - off;
    memcpy(buffer, basep, part1sz);
    memcpy((void*)((uintptr_t)buffer + part1sz), con->outbuf, size - part1sz);
  } else {
    memcpy(buffer, basep, size);
  }
  con->output->produced = (con->output->produced + size) % con->outmod;
  __sync_synchronize();
  xc_evtchn_notify(con->iface->ec, con->port);
}

