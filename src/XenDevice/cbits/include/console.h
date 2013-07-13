// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Magnus Carlsson <magnus@galois.com>
// BANNEREND
#include <xen/io/console.h>

int xencons_can_send(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp,
                     unsigned int bufsize);
int xencons_ring_send(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp,
                      char *shdata, unsigned int evtchn, 
                      unsigned int bufsize, const char *data, int len);
int xencons_can_receive(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp);
int xencons_ring_receive(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp,
                         const char *shdata, unsigned int evtchn,
                         unsigned int bufsize, char *data, int len);
unsigned long get_console_mfn(void);
unsigned int get_console_evtchn(void);

