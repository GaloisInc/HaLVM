/* -*-  Mode:C; c-basic-offset:4; tab-width:4 -*-
 ****************************************************************************
 * (C) 2003 - Rolf Neugebauer - Intel Research Cambridge
 * (C) 2005 - Grzegorz Milos - Intel Research Cambridge
 ****************************************************************************
 *
 *        File: time.h
 *      Author: Rolf Neugebauer (neugebar@dcs.gla.ac.uk)
 *     Changes: Grzegorz Milos (gm281@cam.ac.uk)
 *              
 *        Date: Jul 2003, changesJun 2005
 * 
 * Environment: Xen Minimal OS
 * Description: Time and timer functions
 *
 ****************************************************************************
 */

#ifndef _TIME_H_
#define _TIME_H_

#include <types.h>

/*
 * System Time
 * 64 bit value containing the nanoseconds elapsed since boot time.
 * This value is adjusted by frequency drift.
 * NOW() returns the current time.
 * The other macros are for convenience to approximate short intervals
 * of real time into system time 
 */
typedef s64 s_time_t;
#define NOW()                   ((s_time_t)monotonic_clock())
#define SECONDS(_s)             (((s_time_t)(_s))  * 1000000000UL )
#define TENTHS(_ts)             (((s_time_t)(_ts)) * 100000000UL )
#define HUNDREDTHS(_hs)         (((s_time_t)(_hs)) * 10000000UL )
#define MILLISECS(_ms)          (((s_time_t)(_ms)) * 1000000UL )
#define MICROSECS(_us)          (((s_time_t)(_us)) * 1000UL )
#define Time_Max                ((s_time_t) 0x7fffffffffffffffLL)
#define FOREVER                 Time_Max
#define NSEC_TO_USEC(_nsec)     (_nsec / 1000UL)
#define NSEC_TO_SEC(_nsec)      (_nsec / 1000000000ULL)

/* wall clock time  */
typedef long time_t;
typedef long suseconds_t;
struct timeval {
	time_t		tv_sec;		/* seconds */
	suseconds_t	tv_usec;	/* microseconds */
};

struct timespec {
    time_t      ts_sec;
    long        ts_nsec;
};

struct tm { 
	int			 tm_sec;
	int			 tm_min;
	int			 tm_hour;
	int			 tm_mday;
	int			 tm_mon;
	int			 tm_year;
	int			 tm_wday;
	int			 tm_yday;
	int			 tm_isdst;
	int			 tm_gmtoff;
	__const char *tm_zone;
};

struct rusage {
	struct timeval ru_utime; /* user time used */
	struct timeval ru_stime; /* system time used */
	long   ru_maxrss;        /* maximum resident set size */
	long   ru_ixrss;         /* integral shared memory size */
	long   ru_idrss;         /* integral unshared data size */
	long   ru_isrss;         /* integral unshared stack size */
	long   ru_minflt;        /* page reclaims */
	long   ru_majflt;        /* page faults */
	long   ru_nswap;         /* swaps */
	long   ru_inblock;       /* block input operations */
	long   ru_oublock;       /* block output operations */
	long   ru_msgsnd;        /* messages sent */
	long   ru_msgrcv;        /* messages received */
	long   ru_nsignals;      /* signals received */
	long   ru_nvcsw;         /* voluntary context switches */
	long   ru_nivcsw;        /* involuntary context switches */
};

/* prototypes */
void       init_time(void);
int        gettimeofday(struct timeval *tv,void *ignore);
u32        block_domain(u32 millisecs);
u64        monotonic_clock(void);
struct tm *localtime_r(const time_t *timep, struct tm *result);
struct tm *gmtime_r(const time_t *timep, struct tm *result);
int		   getrusage(int who, struct rusage *usage);
time_t     mktime(struct tm *tm);
time_t     time(time_t *t);
char      *ctime_r(const time_t *timep, char *buf);

#define RUSAGE_SELF 0
#endif /* _TIME_H_ */
