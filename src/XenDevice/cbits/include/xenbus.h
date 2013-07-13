// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Magnus Carlsson <magnus@galois.com>
// BANNEREND
#include <errno.h>
#include "types.h"
#include "xen/io/xs_wire.h"

int xb_can_write(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp);
int xb_write(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp,
             char *shdata, unsigned int store_evtchn,
             const void *data, int len);
int xb_can_read(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp);
int xb_read(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp,
            const char *shdata, unsigned int store_evtchn, 
            void *data, int len);
unsigned int get_store_evtchn();
unsigned long get_store_mfn();
void init_xenbus(void);
unsigned long p2round(unsigned long x);
