/*  Derived in part from mini-os

 ****************************************************************************
 * (C) 2003 - Rolf Neugebauer - Intel Research Cambridge
 * (C) 2005 - Grzegorz Milos - Intel Reseach Cambridge
 ****************************************************************************
 *
 *        File: events.h
 *      Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
 *     Changes: Grzegorz Milos (gm281@cam.ac.uk)
 *              
 *        Date: Jul 2003, changes Jun 2005
 * 
 * Environment: Xen Minimal OS
 * Description: Deals with events on the event channels
 *
 ****************************************************************************
 */

#ifndef _EVENTS_H_
#define _EVENTS_H_

#include <traps.h>
#include <xen/xen.h>
#include <xen/event_channel.h>
#include <types.h>
#include "Rts.h"
#include "rts/sm/GC.h"

/* prototypes */
void mark_event_handlers(evac_fn evac, void *user);
void do_event(u32 port, struct pt_regs *regs);
s32 bind_virq(u32 virq, u32 vcpu);
s32 bind_pirq(u32 pirq, int share);
void bind_evtchn( u32 port, void (*c_handler)(int, struct pt_regs *) );
void unbind_evtchn( u32 port );
void init_events(void);
void unbind_virq( u32 port );
s32 evtchn_alloc_unbound(u32 for_domain, u32 remote_dom) ;
s32 evtchn_bind_interdomain(u32 remote_dom, u32 remote_port) ;
s32 evtchn_close(u32 port);
s32 evtchn_send(u32 port);
void mask_evtchn(u32 port);
void unmask_evtchn(u32 port);
void clear_evtchn(u32 port);
void clear_event_handlers(void);
void do_hypervisor_callback(struct pt_regs *);
int pause(void);
u32 irq_get_status(u32);
void irq_send_eoi(u32);

void set_port_handler(unsigned int port, StgStablePtr sp);
StgStablePtr unset_port_handler(unsigned int port);

int notify_remote_via_evtchn(int port);

extern void make_handler_pending(StgStablePtr hsp);

#endif /* _EVENTS_H_ */

