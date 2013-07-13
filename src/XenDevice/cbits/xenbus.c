// Derived from mini-os

/******************************************************************************
 * xenbus_comms.c
 *
 * Low level code to talks to Xen Store: ringbuffer and event channel.
 *
 * Copyright (C) 2005 Rusty Russell, IBM Corporation
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License version 2
 * as published by the Free Software Foundation; or, when distributed
 * separately from the Linux kernel or incorporated into other
 * software packages, subject to the following license:
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this source file (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include <Rts.h>
#include <types.h>
#include <events.h>
#include <errno.h>
#include <xen/io/xs_wire.h>
#include <xen/io/ring.h>
#include <xenbus.h>
#include <mm.h>
#include <hbmxen.h>
#include <arch.h>
#include <string.h>

static int check_indexes(XENSTORE_RING_IDX cons, XENSTORE_RING_IDX prod)
{
	return ((prod - cons) <= XENSTORE_RING_SIZE);
}


static void *get_output_chunk(XENSTORE_RING_IDX cons,
			      XENSTORE_RING_IDX prod,
			      char *buf, uint32_t *len)
{
	*len = XENSTORE_RING_SIZE - MASK_XENSTORE_IDX(prod);
	if ((XENSTORE_RING_SIZE - (prod - cons)) < *len)
		*len = XENSTORE_RING_SIZE - (prod - cons);
	return buf + MASK_XENSTORE_IDX(prod);
}

static const void *get_input_chunk(XENSTORE_RING_IDX cons,
				   XENSTORE_RING_IDX prod,
				   const char *buf, uint32_t *len)
{
	*len = XENSTORE_RING_SIZE - MASK_XENSTORE_IDX(cons);
	if ((prod - cons) < *len)
		*len = prod - cons;
	return buf + MASK_XENSTORE_IDX(cons);
}





int xb_can_write(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp)
{
	return ((*prodp - *consp) != XENSTORE_RING_SIZE);
}

// attempt to write len bytes; return number actually written
// non-blocking
int xb_write(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp,
             char *shdata, unsigned int store_evtchn,
             const void *data, int len)
{
	XENSTORE_RING_IDX cons, prod;
	int rc;

	void *dst;
	unsigned int avail;

	
	/* Read indexes, then verify. */
	cons = *consp;
	prod = *prodp;
	mb();
	if (!check_indexes(cons, prod)) {
	  *consp = *prodp = 0;
	  pabort ("xb_write: xenbus ring invariant violated");
	}

	dst = get_output_chunk(cons, prod, shdata, &avail);
	if (avail == 0)
	  return 0;
	if (avail > len)
	  avail = len;

	memcpy(dst, data, avail);
	data += avail;
	len -= avail;

	/* Other side must not see new header until data is there. */
	wmb();
	*prodp += avail;

	/* This implies mb() before other side sees interrupt. */
	notify_remote_via_evtchn(store_evtchn);

	return avail;
}

int xb_can_read(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp)
{
	return (*consp != *prodp);
}

// attempt to read len bytes; return number actually read
// non-blocking
int xb_read(XENSTORE_RING_IDX *prodp, XENSTORE_RING_IDX *consp,
            const char *shdata, unsigned int store_evtchn, 
            void *data, int len)
{
	XENSTORE_RING_IDX cons, prod;
	int rc;

	unsigned int avail;
	const char *src;

	/* Read indexes, then verify. */
	cons = *consp;
	prod = *prodp;
	mb();
	if (!check_indexes(cons, prod)) {
	  *consp = *prodp = 0;
	  pabort ("xb_read: xenbus ring invariant violated");
	}

	src = get_input_chunk(cons, prod, shdata, &avail);
	if (avail == 0)
	  return 0;
	if (avail > len)
	  avail = len;

	/* We must read header before we read data.  ??? APT */
	rmb();

	memcpy(data, src, avail);
	data += avail;
	len -= avail;

	/* Other side must not see free space until we've copied out */
	mb();
	*consp += avail;


	/* Implies mb(): they will see new header. */
	notify_remote_via_evtchn(store_evtchn);

	return avail;
}

// shouldn't really be a function
unsigned int get_store_evtchn() {
  return start_info->store_evtchn;
}

unsigned long get_store_mfn() {
  return start_info->store_mfn;
}

void init_xenbus(void) {
  // nothing to do for now
  // (I don't think we want a default handler...)
}

unsigned long p2round(unsigned long x)
{
  return __RD32(x);
}
