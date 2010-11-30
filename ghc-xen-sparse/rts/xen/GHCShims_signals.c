// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com> and Andrew Tolmach <apt@galois.com>
// BANNEREND
#include "Rts.h"
#include "Schedule.h"
#include "RtsSignals.h"
//#include "SchedAPI.h"
#include "arch.h"
#include "events.h"
#include "hypercall.h"

#define MAX_PENDING_HANDLERS 1024

// Below to remove warnings.
rtsBool anyUserHandlers(void);
// Above to remove warnings.
#ifndef ALLOW_INTERPRETER
int rts_stop_on_exception    = 0;
int rts_breakpoint_io_action = 0;
#endif

StgStablePtr  pending_handler_buf[MAX_PENDING_HANDLERS];
int           next_pending_handler = 0;

void initUserSignals(void)
{
  int i;

  for(i = 0; i < MAX_PENDING_HANDLERS; i++)
    pending_handler_buf[i] = NULL;
  next_pending_handler = 0;
}

void initDefaultHandlers(void)
{
  return; /* No action for the HaLVM */
}

void resetDefaultHandlers(void)
{
  return; /* Again, no action for the HaLVM */
}

void freeSignalHandlers(void)
{
  return; /* Again, again, no action for the HaLVM */
}

int signals_pending(void)
{
  return next_pending_handler;
}

rtsBool anyUserHandlers(void)
{
  return rtsTrue; // There's always at least one.
}

/******************************************************************************/

void make_handler_pending(StgStablePtr hsp)
{
  if(next_pending_handler == MAX_PENDING_HANDLERS)
    pabort("Too many pending signals!");
  pending_handler_buf[next_pending_handler++] = hsp;
}

int stg_sig_install(int sig, int spi, void *h __attribute__((unused)))
{
  switch(sig) {
    case 2: /* SIGINT */
      return spi;
    default:
      pabort("Someone called stg_sig_install for signal %i!\n", sig);
      return STG_SIG_ERR;
  }
}

void awaitUserSignals(void)
{
  while(!signals_pending() && sched_state == SCHED_RUNNING) {
    pause();
  }
}

void startSignalHandlers(Capability *c)
{
  int i;

  __cli();
  for(i = 0; i < next_pending_handler; i++) {
    StgClosure *h = (StgClosure*)deRefStablePtr(pending_handler_buf[i]);
    scheduleThread(c, createIOThread(c, RtsFlags.GcFlags.initialStkSize, h));
  }
  next_pending_handler = 0;
  __sti();
}

/******************************************************************************/

void blockUserSignals(void)
{
}

void unblockUserSignals(void)
{
}

/******************************************************************************/

void markSignalHandlers(evac_fn evac __attribute__((unused)),
                        void *user __attribute__((unused)))
{
}

