/* Derived in part from mini-os

****************************************************************************
* (C) 2003 - Rolf Neugebauer - Intel Research Cambridge
* (C) 2005 - Grzegorz Milos - Intel Research Cambridge
****************************************************************************
*
*        File: events.c
*      Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
*     Changes: Grzegorz Milos (gm281@cam.ac.uk)
*              
*        Date: Jul 2003, changes Jun 2005
* 
* Environment: Xen Minimal OS
* Description: Deals with events recieved on event channels
*
****************************************************************************
*/

#include <Rts.h>
#include <Signals.h>
#include <Schedule.h>
#include <sm/GC.h>
#include <events.h>
#include <stdlib.h>
#include <arch.h>
#include <hypercall.h>
#include <time.h>
#include <strings.h>

#include <xen/physdev.h>

#define NR_EVS 1024
#define IRQ_STACK_SIZE (4 * PAGE_SIZE)

/* This represents a event handler, which can have either a C handler or a 
   Haskell handler, or both  */
typedef struct _ev_action_t {
  void (*c_handler)(int, struct pt_regs *);
  StgStablePtr haskell_handler;
  u32 count;
} ev_action_t;

static ev_action_t ev_actions[NR_EVS];

void clear_event_handlers()
{
  // Completely wipes the event handlers structure. This should only be
  // called directly before a crash.
  bzero(ev_actions, sizeof(ev_actions));
  printf("Event handlers cleared!\n");
}

inline void clear_evtchn(u32 port)
{
    shared_info_t *s = HYPERVISOR_shared_info;
    synch_clear_bit(port, &s->evtchn_pending[0]);
}

// Mark the haskell handlers
void mark_event_handlers(evac_fn evac, void *user)
{
  int i; 

  for(i = 0; i < NR_EVS; i++)
    if(ev_actions[i].haskell_handler)
      evac(user, (StgClosure**)&(ev_actions[i].haskell_handler));
}

/*
 * Demux events to different handlers.
 */
void do_event(u32 port, struct pt_regs *regs)
{
  ev_action_t  *action;
  if (port >= NR_EVS) 
    pabort("Port number too large: %d\n", port);

  action = &ev_actions[port];
  action->count++;
    
  /* call the C handler, if any */
  if (action->c_handler) action->c_handler(port, regs);

  /* enqueue the Haskell handler, if any */
  if (action->haskell_handler != 0) {  
    // ^-- as it happens, a valid StgStablePtr is never 0
    make_handler_pending(action->haskell_handler);
  }

  clear_evtchn(port);
}

inline void mask_evtchn(u32 port)
{
    shared_info_t *s = HYPERVISOR_shared_info;
    synch_set_bit(port, &s->evtchn_mask[0]);
}

inline void unmask_evtchn(u32 port)
{
    shared_info_t *s = HYPERVISOR_shared_info;
    vcpu_info_t *vcpu_info = &s->vcpu_info[smp_processor_id()];

    synch_clear_bit(port, &s->evtchn_mask[0]);

    /*
     * The following is basically the equivalent of 'hw_resend_irq'. Just like
     * a real IO-APIC we 'lose the interrupt edge' if the channel is masked.
     */
    if (  synch_test_bit        (port,    &s->evtchn_pending[0]) && 
         !synch_test_and_set_bit(port>>5, &vcpu_info->evtchn_pending_sel) )
    {
        vcpu_info->evtchn_upcall_pending = 1;
        if ( !vcpu_info->evtchn_upcall_mask )
            force_evtchn_callback();
    }
}

#define active_evtchns(cpu,sh,idx)              \
    ((sh)->evtchn_pending[idx] &                \
     ~(sh)->evtchn_mask[idx])

// attach a Haskell handler to a port
void set_port_handler(unsigned int port, StgStablePtr sp) 
{
  if (port >= NR_EVS) 
    pabort("Port number too large: %d\n", port);

  ev_actions[port].haskell_handler = sp;
  unmask_evtchn(port);
}

StgStablePtr unset_port_handler(unsigned int port) 
{
  StgStablePtr retval = ev_actions[port].haskell_handler;
  ev_actions[port].haskell_handler = NULL;
  mask_evtchn(port);
  return retval;
}

// attach a C handler to a port
void bind_evtchn( u32 port, void (*c_handler)(int, struct pt_regs *) )
{
  if (port >= NR_EVS) 
    pabort("Port number too large: %d\n", port);

  if(ev_actions[port].c_handler)
    printf("EV: WARN: Handler for port %d already registered, replacing\n",
	   port);

  ev_actions[port].c_handler = c_handler;
 
  /* Finally unmask the port */
  unmask_evtchn(port);
}

void unbind_evtchn( u32 port )
{
  if (ev_actions[port].c_handler == 0)
    printf("EV: WARN: No handler for port %d when unbinding\n", port);
  ev_actions[port].c_handler = 0;
}

s32 bind_pirq(u32 irq, int shared)
{
  evtchn_op_t op;
  int err;

  /* XXX: Should we check that the IRQ is in range,
   * or does XEN take care of that?
   */
  bzero(&op, sizeof(op));
  op.cmd = EVTCHNOP_bind_pirq;
  op.u.bind_pirq.pirq = irq;
  op.u.bind_pirq.flags = shared? BIND_PIRQ__WILL_SHARE : 0;

  err = HYPERVISOR_event_channel_op(&op);
  if (err)
    return err;

  return op.u.bind_pirq.port;
}

s32 bind_virq(u32 virq, u32 vcpu)
{
  evtchn_op_t op;
  int err;

  /* Try to bind the virq to a port */
  op.cmd = EVTCHNOP_bind_virq;
  op.u.bind_virq.virq = virq;
  op.u.bind_virq.vcpu = vcpu;

  err = HYPERVISOR_event_channel_op(&op);
  if (err)
    return err;
  return op.u.bind_virq.port;
}

void unbind_virq( u32 port )
{
  unbind_evtchn(port);
}

s32 evtchn_alloc_unbound(u32 for_domain, u32 remote_dom) 
{
  evtchn_op_t op;
  int err;

  op.cmd = EVTCHNOP_alloc_unbound;
  op.u.alloc_unbound.dom = for_domain;
  op.u.alloc_unbound.remote_dom = remote_dom;

  err = HYPERVISOR_event_channel_op(&op);
  if (err) {
    printf("EV: Failed to alloc unbound evtchn: %d.\n", err);
    return err;
  }
  return op.u.alloc_unbound.port;
}

s32 evtchn_bind_interdomain(u32 remote_dom, u32 remote_port) 
{
  evtchn_op_t op;
  int err;

  op.cmd = EVTCHNOP_bind_interdomain;
  op.u.bind_interdomain.remote_dom = remote_dom;
  op.u.bind_interdomain.remote_port = remote_port;

  err = HYPERVISOR_event_channel_op(&op);
  if (err) {
    printf("EV: Failed to bind interdomain %d.\n", err);
    return err;
  }
  return op.u.bind_interdomain.local_port;
}

s32 evtchn_close(u32 port) 
{
  evtchn_op_t op;
  int err;
	
  op.cmd = EVTCHNOP_close;
  op.u.close.port = port;
	
  err = HYPERVISOR_event_channel_op(&op);
  return err; 
}
			
s32 evtchn_send(u32 port) 
{
  evtchn_op_t op;
  int err;
	
  op.cmd = EVTCHNOP_send;
  op.u.send.port = port;

  err = HYPERVISOR_event_channel_op(&op);
  return err;
}
	
int notify_remote_via_evtchn(int port) 
{
  evtchn_op_t op;
  op.cmd = EVTCHNOP_send;
  op.u.send.port = port;
  return HYPERVISOR_event_channel_op(&op);
}

#if defined(__x86_64__)
char irqstack[2 * IRQ_STACK_SIZE];

static struct pda
{
    int irqcount;       /* offset 0 (used in x86_64.S) */
    char *irqstackptr;  /*        8 */
} cpu0_pda;
#endif


/*
 * Initially all events are without a handler and disabled
 */
void init_events(void)
{
  int i;
#if defined(__x86_64__)
    asm volatile("movl %0,%%fs ; movl %0,%%gs" :: "r" (0));
    wrmsrl(0xc0000101, &cpu0_pda); /* 0xc0000101 is MSR_GS_BASE */
    cpu0_pda.irqcount = -1;
    cpu0_pda.irqstackptr = (void*) (((unsigned long)irqstack + 2 * IRQ_STACK_SIZE)
                                    & ~(IRQ_STACK_SIZE - 1));
#endif
  /* inintialise event handler */
  for ( i = 0; i < NR_EVS; i++ )
    {
      ev_actions[i].c_handler = 0;
      ev_actions[i].haskell_handler = 0; // as it happens, a valid StgStablePtr is never 0
      ev_actions[i].count = 0;
      mask_evtchn(i);
    }
}

void do_hypervisor_callback(struct pt_regs *regs)
{
    unsigned long l1, l2;
    unsigned int   l1i, l2i, port;
    int            cpu = 0;
    shared_info_t *s = HYPERVISOR_shared_info;
    vcpu_info_t   *vcpu_info = &s->vcpu_info[cpu];

    vcpu_info->evtchn_upcall_pending = 0;
    /* NB. No need for a barrier here -- XCHG is a barrier on x86. */
    l1 = xenxchg(&vcpu_info->evtchn_pending_sel, 0);
    while ( l1 != 0 )
    {
        l1i = __ffs(l1);
        l1 &= ~(1 << l1i);
        
        while ( (l2 = active_evtchns(cpu, s, l1i)) != 0 )
        {
            l2i = __ffs(l2);
            l2 &= ~(1 << l2i);

            port = (l1i * (sizeof(unsigned long) * 8)) + l2i;
            do_event(port, regs);
        }
    }
}

void force_evtchn_callback(void)
{
  vcpu_info_t *vcpu = &HYPERVISOR_shared_info->vcpu_info[0];
  int save = vcpu->evtchn_upcall_mask;
  
  while(vcpu->evtchn_upcall_pending) {
    vcpu->evtchn_upcall_mask = 1;
    barrier();
    do_hypervisor_callback(NULL);
    barrier();
    vcpu->evtchn_upcall_mask = save;
    barrier();
  }
}

extern int signals_pending(void);

int pause(void) 
{
  // All the ifs / clis / forces are due to obscene little race conditions,
  // where if we're not very careful, we lose events.
  if(!signals_pending()) {
    __cli();
    force_evtchn_callback();
    if(!signals_pending()) {
      HYPERVISOR_sched_op(SCHEDOP_block, 0);
      force_evtchn_callback();
    } else __sti();
  }
  return -1;
}

#if defined(__x86_64__)
u32 irq_get_status(u32 irq)
{
  struct physdev_irq_status_query op;

  bzero(&op, sizeof(struct physdev_irq_status_query));
  op.irq = irq;
  assert(!HYPERVISOR_physdev_op(PHYSDEVOP_irq_status_query, &op));
  return op.flags;
}

void irq_send_eoi(u32 irq)
{
  struct physdev_eoi op;

  bzero(&op, sizeof(struct physdev_eoi));
  op.irq = irq;
  assert(!HYPERVISOR_physdev_op(PHYSDEVOP_eoi, &op));
}

#else
u32 irq_get_status(u32 irq)
{
  struct physdev_op op;

  bzero(&op, sizeof(struct physdev_op));

  op.cmd                    = PHYSDEVOP_irq_status_query;
  op.u.irq_status_query.irq = irq;

  assert(!HYPERVISOR_physdev_op_compat(&op));

  return op.u.irq_status_query.flags;
}

void irq_send_eoi(u32 irq)
{
  struct physdev_op op;

  bzero(&op, sizeof(struct physdev_op));

  // since they don't have a physdev_irq_eoi struct in the union, we have to
  // pass a irq_op, with the right cmd, and have them just default to picking
  // the irq out of the beginning of the union.
  op.cmd          = PHYSDEVOP_eoi;
  op.u.irq_op.irq = irq;

  assert(!HYPERVISOR_physdev_op_compat(&op));
}
#endif
