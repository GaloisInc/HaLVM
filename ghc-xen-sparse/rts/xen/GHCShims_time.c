// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND

#include "Rts.h"
#include "GetTime.h"
#include "Ticker.h"
#include "time.h"

// Below to remove warnings.
lnat getourtimeofday(void);
// Above to remove warnings.

Ticks getProcessCPUTime(void)
{
  return NSEC_TO_USEC(monotonic_clock()) / 1000;
}

Ticks getProcessElapsedTime(void)
{
  return NSEC_TO_USEC(monotonic_clock()) / 1000;
}

void getProcessTimes(Ticks *user, Ticks *elapsed)
{
  *user = NSEC_TO_USEC(monotonic_clock()) / 1000;
  *elapsed = NSEC_TO_USEC(monotonic_clock()) / 1000;
}

/******************************************************************************/

extern volatile TickProc timer0_proc;
       volatile TickProc saved_ticker;

void initTicker  (nat ms __attribute__((unused)), TickProc handle_tick)
{
  saved_ticker = handle_tick;
}

void startTicker (void)
{
  timer0_proc = saved_ticker;
}

void stopTicker  (void)
{
  saved_ticker = timer0_proc;
  timer0_proc = NULL;
}

void exitTicker  (rtsBool wait)
{
  saved_ticker = NULL;
}

/******************************************************************************/

lnat getourtimeofday(void)
{
  static u64 last_time = 0;
  struct timeval tv;
  nat  interval;
  u64  work;

  gettimeofday(&tv, (struct timezone *)NULL);

  interval = RtsFlags.MiscFlags.tickInterval;
  if(interval == 0) { interval = 50; }

  // cast to lnat because nat may be 64 bit when int is only 32 bit
  work  = (u64)tv.tv_sec * (1000 / interval);
  work += (u64)tv.tv_usec / (interval * 1000);
  //assert(last_time <= work);
  last_time = work;
  work &= 0x7FFFFFFF;

  return (lnat)work;
}
