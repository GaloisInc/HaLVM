// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#include <stdlib.h>
#include <stdio.h>
#include <hypercall.h>
#include <events.h>
#include <hbmxen.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <machine/limits.h>

char *getenv(const char *name __attribute__((unused)))
{
  return NULL; // There is no environment, so this always returns "not found"
}

void abort(void)
{
  printf("Abort called!\n");
  // Clear the event handlers and then crash. This is probably too
  // paranoid.
  clear_event_handlers();
  do_exit();
}

__attribute__ ((noreturn))
void pabort(const char *format, ...)
{
  va_list args;
  
  va_start(args, format);
  printf("ABORT: ");
  vfprintf(stderr, format, args);
  va_end(args);
  // Clear the event handlers and then crash. This is probably too
  // paranoid.
  clear_event_handlers();
  do_exit();
}

__attribute__ ((noreturn))
int raise(int sig)
{
  pabort("Someone called raise! (signal: %d)\n", sig);
}

__attribute__ ((noreturn))
void exit(int status)
{
  printf("Exit called with %d\n", status);
  do_exit();
}

#define isdigit(c) (c >= '0' && c <= '9')

double atof(const char *s)
{
  // This function stolen from either Rolf Neugebauer or Andrew Tolmach. 
  // Probably Rolf.
  double a = 0.0;
  int e = 0;
  int c;
  while ((c = *s++) != '\0' && isdigit(c)) {
    a = a*10.0 + (c - '0');
  }
  if (c == '.') {
    while ((c = *s++) != '\0' && isdigit(c)) {
      a = a*10.0 + (c - '0');
      e = e-1;
    }
  }
  if (c == 'e' || c == 'E') {
    int sign = 1;
    int i = 0;
    c = *s++;
    if (c == '+')
      c = *s++;
    else if (c == '-') {
      c = *s++;
      sign = -1;
    }
    while (isdigit(c)) {
      i = i*10 + (c - '0');
      c = *s++;
    }
    e += i*sign;
  }
  while (e > 0) {
    a *= 10.0;
    e--;
  }
  while (e < 0) {
    a *= 0.1;
    e++;
  }
  return a;
}

char *strcpy(char *dest, const char *src)
{
  char *retval = dest;
  // FIXME: Make higher-speed version?
  while(*src) *dest++ = *src++;
  return retval;
}

long int strtol(const char *nptr, char **endptr, int base)
{
  /* Taken from the NetBSD libc implementation. As per the license, here's
   * the copyright notice & etc. attached to it:
   *
   * Copyright (c) 1990, 1993
   *  The Regents of the University of California.  All rights reserved.
   *  Redistribution and use in source and binary forms, with or without
   *  modification, are permitted provided that the following conditions
   *  are met:
   *  1. Redistributions of source code must retain the above copyright
   *     notice, this list of conditions and the following disclaimer.
   *  2. Redistributions in binary form must reproduce the above copyright
   *     notice, this list of conditions and the following disclaimer in the
   *     documentation and/or other materials provided with the distribution.
   *  3. Neither the name of the University nor the names of its contributors
   *     may be used to endorse or promote products derived from this software
   *     without specific prior written permission.
   * 
   * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
   * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
   * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
   * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
   * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
   * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
   * SUCH DAMAGE.
   */
#define isalpha(c)  (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
#define isupper(c)  ((c >= 'A') && (c <= 'Z'))
#define isspace(c)  ((c == ' ') || (c == '\n') || (c == '\t'))
  const char *s;
  long acc, cutoff;
  int c;
  int neg, any, cutlim;

  assert(nptr != NULL);
  /* endptr may be NULL */

  /*
   * Skip white space and pick up leading +/- sign if any.
   * If base is 0, allow 0x for hex and 0 for octal, else
   * assume decimal; if base is already 16, allow 0x.
   */
  s = nptr;
  do {
    c = (unsigned char) *s++;
  } while (isspace(c));
  if (c == '-') {
    neg = 1;
    c = *s++;
  } else {
    neg = 0;
    if (c == '+')
      c = *s++;
  } 
  if ((base == 0 || base == 16) && c == '0' && (*s == 'x' || *s == 'X')) {
    c = s[1];
    s += 2;
    base = 16;
  }
  if (base == 0)
    base = c == '0' ? 8 : 10;

  /*
   * Compute the cutoff value between legal numbers and illegal
   * numbers.  That is the largest legal value, divided by the
   * base.  An input number that is greater than this value, if
   * followed by a legal input character, is too big.  One that
   * is equal to this value may be valid or not; the limit
   * between valid and invalid numbers is then based on the last
   * digit.  For instance, if the range for longs is
   * [-2147483648..2147483647] and the input base is 10,
   * cutoff will be set to 214748364 and cutlim to either
   * 7 (neg==0) or 8 (neg==1), meaning that if we have accumulated
   * a value > 214748364, or equal but the next digit is > 7 (or 8),
   * the number is too big, and we will return a range error.
   *
   * Set any if any `digits' consumed; make it negative to indicate
   * overflow.
   */
  cutoff = neg ? LONG_MIN : LONG_MAX;
  cutlim = (int)(cutoff % base);
  cutoff /= base;
  if (neg) {
    if (cutlim > 0) {
      cutlim -= base;
      cutoff += 1;
    }
    cutlim = -cutlim;
  }
  for (acc = 0, any = 0;; c = (unsigned char) *s++) {
    if (isdigit(c))
      c -= '0';
    else if (isalpha(c))
      c -= isupper(c) ? 'A' - 10 : 'a' - 10;
    else
      break;
    if (c >= base)
      break;
    if (any < 0)
      continue;
    if (neg) {
      if (acc < cutoff || (acc == cutoff && c > cutlim)) {
        any = -1;
        acc = LONG_MIN;
        errno = ERANGE;
      } else {
        any = 1;
        acc *= base;
        acc -= c;
      }
    } else {
      if (acc > cutoff || (acc == cutoff && c > cutlim)) {
        any = -1;
        acc = LONG_MAX;
        errno = ERANGE;
      } else {
        any = 1;
        acc *= base;
        acc += c;
      }
    }
  }
  if (endptr != 0)
    *endptr = (char*)(any ? s - 1 : nptr);
  return (acc);
}

void *bsearch(const void *key,
    	      const void *base0,
	          size_t nmemb,
	          size_t size,
	          int (*compar)(const void *, const void *))
{
  const char *base = base0;
  size_t lim;
  int cmp;
  const void *p;

  for (lim = nmemb; lim != 0; lim >>= 1) {
    p = base + (lim >> 1) * size;
    cmp = (*compar)(key, p);
    if (cmp == 0)
      /* LINTED interface spec */
      return ((void *)p);
    if (cmp > 0) {  /* key > p: move right */
      /* LINTED we don't touch base */
      base = (char *)p + size;
      lim--;
    }               /* else move left */
  }
  return (NULL);
}

long sysconf(int name)
{
  switch(name) {
    case _SC_PAGESIZE:
      return 4096;
    case _SC_CLK_TCK:
      printf("i was called.\n");
      return 100; /* FIXME: number of clock ticks per second */
  }

  return 0;
}

int mkstemp(char *template __attribute__ ((unused))) {
  return (-1);
}
