/* 
 * Derived from mini-os
 *
 ****************************************************************************
 * (C) 2006 - Grzegorz Milos - Cambridge University
 ****************************************************************************
 *
 *        File: console.h, xencons_ring.c
 *      Author: Grzegorz Milos
 *     Changes: 
 *              
 *        Date: Mar 2006
 * 
 * Environment: Xen Minimal OS
 * Description: Console interface.
 *
 * Handles console I/O. Defines printk.
 *
 ****************************************************************************
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
 * DEALINGS IN THE SOFTWARE.
 */
#include <Rts.h>
#include <mm.h>
#include <events.h>
#include <console.h>
#include <hbmxen.h>
#include <xen/io/console.h>
#include <arch.h>
#include <stdio.h>
#include <string.h>

unsigned long get_console_mfn()
{
    return start_info->console_mfn;
}

int xencons_can_receive(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp) {
  return (*consp != *prodp);
}

// receives up to len characters; returns number actually received
int xencons_ring_receive(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp,
                         const char *shdata, unsigned int evtchn,
                         unsigned int bufsize, char *data, int len) {
  int rcvd = 0;
  const char _shdata[bufsize];
  XENCONS_RING_IDX cons, prod;

  cons = *consp;
  prod = *prodp;
  mb();
  if ((prod - cons) > bufsize) 
    pabort("xencons_ring_receive: console ring invariant violated");
  
  while ((rcvd < len) && (cons != prod)) 
    data[rcvd++] = *(shdata+MASK_XENCONS_IDX(cons++,_shdata));
  
  mb();
  *consp = cons;
  
  notify_remote_via_evtchn(evtchn);
  return rcvd;
}


int xencons_can_send(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp,
                     unsigned int bufsize) {
  return (*prodp - *consp) < bufsize;
}

// sends up to len characters; returns number actually sent
int xencons_ring_send(XENCONS_RING_IDX *prodp, XENCONS_RING_IDX *consp,
                      char *shdata, unsigned int evtchn, 
                      unsigned int bufsize, const char *data, int len)
{	
    int sent = 0;
    const char _shdata[bufsize];
    XENCONS_RING_IDX cons, prod;

    cons = *consp;
    prod = *prodp;
    mb();
    if ((prod - cons) > bufsize)
      pabort("xencons_ring_send: console ring invariant violated");
    
    while ((sent < len) && ((prod - cons) < bufsize))
      shdata[MASK_XENCONS_IDX(prod++,_shdata)] = data[sent++];
    
    wmb();
    *prodp = prod;
    notify_remote_via_evtchn(evtchn);
    return sent;
}

// shouldn't really be a function
unsigned int get_console_evtchn() 
{
  return start_info->console_evtchn;
}
