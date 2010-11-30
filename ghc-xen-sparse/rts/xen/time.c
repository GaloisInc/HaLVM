// (ACW) This code is mostly taken from mini-os, and then modified/extended for
// the purposes of the HALVM. The minios file attached the following copyright
// message:
/* -*-  Mode:C; c-basic-offset:4; tab-width:4 -*-
 ****************************************************************************
 * (C) 2003 - Rolf Neugebauer - Intel Research Cambridge
 * (C) 2002-2003 - Keir Fraser - University of Cambridge 
 * (C) 2005 - Grzegorz Milos - Intel Research Cambridge
 * (C) 2006 - Robert Kaiser - FH Wiesbaden
 ****************************************************************************
 *
 *        File: time.c
 *      Author: Rolf Neugebauer and Keir Fraser
 *     Changes: Grzegorz Milos
 *
 * Description: Simple time and timer functions
 *
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
#include <time.h>
#include <arch.h>
#include <hypercall.h>
#include <events.h>
#include <stdio.h>
#include <strings.h>
#include <hbmxen.h>

/* These are peridically updated in shared_info, and then copied here. */
struct shadow_time_info {
  u64 tsc_timestamp;     /* TSC at last update of time vals.  */
  u64 system_timestamp;  /* Time, in nanosecs, since boot.    */
  u32 tsc_to_nsec_mul;
  u32 tsc_to_usec_mul;
  int tsc_shift;
  u32 version;
};

static struct timespec shadow_ts = { 0, 0 };
static u32 shadow_ts_version = 0;
static struct shadow_time_info shadow = { 0, 0, 0, 0, 0, 0 };
static u64 start_time = 0;

#define HANDLE_USEC_OVERFLOW(_tv)           \
    do {                                    \
        while ( (_tv)->tv_usec >= 1000000 ) \
        {                                   \
            (_tv)->tv_usec -= 1000000;      \
            (_tv)->tv_sec++;                \
        }                                   \
    } while ( 0 )

static inline int time_values_up_to_date(void)
{
  struct vcpu_time_info *src = &HYPERVISOR_shared_info->vcpu_info[0].time; 
  return (shadow.version == src->version);
}

static u64 get_nsec_offset(void)
{
  u64 now, delta, shift;
  rdtscll(now);

  delta = now - shadow.tsc_timestamp;
  
  if (shadow.tsc_shift < 0)
     shift = delta >> -(shadow.tsc_shift);
  else
     shift = delta << shadow.tsc_shift;

  return (shift * shadow.tsc_to_nsec_mul) >> 32;
}

static void get_time_values_from_xen(void)
{
  struct vcpu_time_info    *src = &HYPERVISOR_shared_info->vcpu_info[0].time;

  do {
    shadow.version = src->version;
    rmb();
    shadow.tsc_timestamp     = src->tsc_timestamp;
    shadow.system_timestamp  = src->system_time;
    shadow.tsc_to_nsec_mul   = src->tsc_to_system_mul;
    shadow.tsc_shift         = src->tsc_shift;
    rmb();
  } while ((src->version & 1) | (shadow.version ^ src->version));
  shadow.tsc_to_usec_mul = shadow.tsc_to_nsec_mul / 1000;
}

/* monotonic_clock(): returns # of nanoseconds passed since time_init()
 *		Note: This function is required to return accurate
 *		time even in the absence of multiple timer ticks.
 */
u64 monotonic_clock(void)
{
  u64 time;
  u32 local_time_version;

  do {
    local_time_version = shadow.version;
    rmb();
    time = shadow.system_timestamp + get_nsec_offset();
    if (!time_values_up_to_date())
      get_time_values_from_xen();
    rmb();
  } while (local_time_version != shadow.version);

  return time;
}

static void update_wallclock(void)
{
  shared_info_t *s = HYPERVISOR_shared_info;

  do {
    shadow_ts_version = s->wc_version;
    rmb();
    shadow_ts.ts_sec  = s->wc_sec;
    shadow_ts.ts_nsec = s->wc_nsec;
    rmb();
  } while ((s->wc_version & 1) | (shadow_ts_version ^ s->wc_version));
}

int gettimeofday(struct timeval *tv, void *ignore __attribute__((unused)))
{
  u64 nsec = monotonic_clock();

  tv->tv_sec   = shadow_ts.ts_sec;
  tv->tv_sec  += NSEC_TO_SEC(nsec);
  tv->tv_usec  = NSEC_TO_USEC(shadow_ts.ts_nsec);
  tv->tv_usec += NSEC_TO_USEC(nsec % 1000000000UL);

  return 0;
}

u32 block_domain(u32 millisecs) // s_time_t until)
{
  // GHC calls this with zero a lot, so we just catch that and return early.
  if(millisecs == 0) {
    return 0;
  }

  // Basically, understand that signals are pretty much the most important
  // thing in the universe to us. So we will not block at all if there are
  // signals. Note also that there are some really unfortunate race 
  // conditions that exist around signals. 
  if(!signals_pending()) {
    __cli();
    force_evtchn_callback();
    if(!signals_pending()) {
      u64 now, until;

      now = monotonic_clock();
      until = now + MILLISECS(millisecs);
      if(monotonic_clock() < until) {
        HYPERVISOR_set_timer_op(until);
        HYPERVISOR_sched_op(SCHEDOP_block, 0);
        // SCHEDOP_block will do our __sti for us.
        force_evtchn_callback();
        now = monotonic_clock();
        if(now > until)
          return 0;
        else
          return NSEC_TO_USEC(until - now) / 1000;
      } else __sti();
    } else __sti(); 
  }
  return 0;
}

// Haskell ticker support
typedef void (*TickProc)(int);
volatile TickProc timer0_proc = NULL;

static void timer_handler(int ev __attribute__((unused)), 
			  struct pt_regs *regs __attribute__((unused)))
{
  get_time_values_from_xen();
  update_wallclock();
  if (timer0_proc)
    timer0_proc(0);
}

void init_time(void)
{
  s32 port;
  get_time_values_from_xen();
  update_wallclock();
  start_time = monotonic_clock();
  port = bind_virq(VIRQ_TIMER, smp_processor_id());
  if (port == -1)
    pabort("Failed to bind virtual IRQ %d\n", VIRQ_TIMER);
  bind_evtchn(port, &timer_handler);
}

// Converting time counts to human-understandable values

static const int days_per_month[2][12] = {
  { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
  { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

static const int days_per_year[2] = { 365, 366 };

static int is_leap_year(int year)
{
  // A years is a leap year if it is divisible by 4 except when the year
  // is divisible by 100 but not 400.
  return (year % 4 == 0) && !( (year % 100 == 0) && (year % 400 != 0) );
}

/*                        sec  min  hr   days */
#define secs_per_year(x) (60 * 60 * 24 * days_per_year[is_leap_year(x)])
#define secs_per_mon(x,y)(60 * 60 * 24 * days_per_month[is_leap_year(x)][y])
#define secs_per_day     (60 * 60 * 24)
#define secs_per_hour    (60 * 60)
#define secs_per_min     (60)

// This code disregards leap seconds.
struct tm *localtime_r(const time_t *time, struct tm *result)
{
  int cur_year = 1970; /* Groovy, man */
  int cur_month = 0, cur_day = 0, cur_hour = 0, cur_min = 0;
  int weekday = 0, yearday = 0;
  int secs_left = *time;
  
  /* Then compute the weekday it is. Jan 1, 1970 was a Thursday. */
  weekday = (4 + (secs_left / secs_per_day)) % 7;

  /* Start pulling off years */
  while(secs_left >= secs_per_year(cur_year)) {
    ++cur_year;
    secs_left -= secs_per_year(cur_year);
  }

  yearday = secs_left / secs_per_day;

  /* Then pull off months */
  while(secs_left >= secs_per_mon(cur_year, cur_month)) {
    ++cur_month;
    secs_left -= secs_per_mon(cur_year, cur_month);
  }

  /* Then pull off days */
  while(secs_left >= secs_per_day) {
    ++cur_day;
    secs_left -= secs_per_day;
  }

  /* Then pull off hours */
  while(secs_left >= secs_per_hour) {
    ++cur_hour;
    secs_left -= secs_per_hour;
  }

  /* Then pull off minutes */
  while(secs_left >= secs_per_min) {
    ++cur_min;
    secs_left -= secs_per_min;
  }
  
  result->tm_sec = secs_left;
  result->tm_min = cur_min;
  result->tm_hour = cur_hour;
  result->tm_mday = cur_day;
  result->tm_mon = cur_month;
  result->tm_year = cur_year - 1900;
  result->tm_wday = weekday;
  result->tm_yday = yearday;
  result->tm_isdst = 0;
  result->tm_gmtoff = 0;
  result->tm_zone = "HALVMST"; /* HALVM Standard Time */

  return result;
}

// Again, this disregards leap seconds. It also assumes that the time
// exported by Xen is GMT, not whatever the local time zone is, because
// there is no time zone support in the HALVM.
struct tm *gmtime_r(const time_t *timep, struct tm *result)
{
  return localtime_r(timep, result);
}

// This is supposed to return detailed information on resource use, but
// is mostly a stub for monotonic clock information. Fun, huh?
int getrusage(int who __attribute__ ((unused)), struct rusage *usage)
{
  u64 cur  = monotonic_clock();
  u64 diff = cur - start_time;

  assert(cur >= start_time);

  bzero(usage, sizeof(struct rusage));
  // We are all users here.
  usage->ru_stime.tv_sec = 0;
  usage->ru_stime.tv_usec = 0;

  usage->ru_utime.tv_sec = NSEC_TO_SEC(diff);
  // Pull out the seconds we've computed above
  diff -= NSEC_TO_SEC(diff) * 1000000000;
  usage->ru_utime.tv_usec = NSEC_TO_USEC(diff);
  usage->ru_maxrss = start_info->nr_pages * 4096;
  usage->ru_ixrss = current_memory_reservation * 4096;
  usage->ru_idrss = current_memory_reservation * 4096;
  usage->ru_isrss = 10 * 4096; // Close enough
  return 0;
}

time_t mktime(struct tm *tm)
{
  int cur_year = 1970; /* Groovy, man */
  int cur_month = 0, cur_day = 0, cur_hour = 0, cur_min = 0;
  time_t work = 0;
  
  while(cur_year < tm->tm_year)
    work += secs_per_year(cur_year++);
  while(cur_month < tm->tm_mon)
    work += secs_per_mon(cur_year, cur_month++);
  while(cur_day++ < tm->tm_mday)
    work += secs_per_day;
  while(cur_hour++ < tm->tm_hour)
    work += secs_per_hour;
  while(cur_min++ < tm->tm_min)
    work += secs_per_min;
  work += tm->tm_sec;

  return work;
}

time_t time(time_t *t)
{
  u64 delta = monotonic_clock();
  time_t retval = shadow_ts.ts_sec;
  retval += NSEC_TO_SEC(delta);
  if(t) *t = retval;
  return retval;
}

char *weekdays[7] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
char *months[12]  = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                      "Aug", "Sep", "Oct", "Nov", "Dec" };

char *ctime_r(const time_t *timep, char *buf)
{
  struct tm tm;
  time_t now = time((time_t*)timep);
  localtime_r(&now, &tm);
  snprintf(buf, 26, "%s %s %02i %02i:%02i:%02i %i\n",
           weekdays[tm.tm_wday], months[tm.tm_mon], tm.tm_mday, tm.tm_hour,
           tm.tm_min, tm.tm_sec, tm.tm_year + 1900);
  return buf;
}
