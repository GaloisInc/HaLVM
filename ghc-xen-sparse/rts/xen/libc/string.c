// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Authors: Adam Wick <awick@galois.com>, Rebekah Leslie <rebekah@galois.com>
// BANNEREND
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <errno.h>
#include <stdio.h>

void  *memcpy(void *dest, const void *src, size_t count)
{
  /* This would be a prime candidate for reimplementation in assembly */
  char *in_src = (char*)src;
  char *in_dest = (char*)dest;

  while(count--)
    *in_dest++ = *in_src++;
  return dest;
}

void  *memmove(void *dest, const void *src, size_t n)
{
  void *d0 = dest;
  char *d = (char *) dest;
  char *s = (char *) src;
  if (s < d)
    for (s += n, d += n; 0 != n; --n)
      *--d = *--s;
  else if (s != d)
    for (; 0 != n; --n)
      *d++ = *s++;
  return d0;
}

int    memcmp(const void *s1, const void *s2, size_t n)
{
   unsigned char *str1 = (void*)s1;
   unsigned char *str2 = (void*)s2;
   size_t pos;

   for(pos = 0; pos < n; pos++) {
     if(str1[pos] < str2[pos])
       return 1;
     if(str1[pos] > str2[pos])
       return -1;
   }
   return 0;
}

int    strcmp(const char *cs, const char *ct)
{
  register signed char __res;

  while (1) {
    if ((__res = *cs - *ct++) != 0 || !*cs++)
      break;
  }
  return __res;
}

int strncmp(const char *str1, const char *str2, size_t count) 
{
  register signed char __res = 0;

  while(count--) {
    if ((__res = *str1 - *str2) != 0 || !*str1++ || !*str2++)
      break;
  }
  return __res;
}

char  *strncpy(char *dest, const char *src, size_t count)
{
  char *tmp = dest;
  
  while (count-- && (*dest++ = *src++) != '\0')
    /* nothing */;
  
  return tmp;
}

void  *memset(void *s,int c, size_t count)
{
  char *xs = (char *) s;

  while (count--)
    *xs++ = c;

  return s;
}

void  *memchr(const void *s, int c, size_t n)
{
  size_t i;

  for(i = 0; i < n; i++)
    if(((unsigned char*)s)[i] == (unsigned char)c)
      return (void*)(s + i);

  return NULL;
}

size_t strnlen(const char *s, size_t count)
{
  const char *sc;

  for (sc = s; count-- && *sc != '\0'; ++sc)
    /* nothing */;
  return sc - s;
}

size_t strlen(const char *s)
{
  const char *sc;

  for (sc = s; *sc != '\0'; ++sc)
    /* nothing */;
  return sc - s;
}

char  *strerror(int errnum)
{
  static char buf[1024];
  switch(errnum) {
    case EBADF: return "Bad file.\n";
    case EACCES: return "Access prohibited.\n";
  }
  sprintf(buf, "Unkown error: %d", errnum);
  return buf;
}

char *strchr(const char *s, int c)
{
  const char *cur;
  for (cur = s; *cur; cur++) {
    if (*cur == c) {
      return ((char*)cur);
    }
  }

  return NULL;
}

char  *strrchr(const char *s, int c)
{
  char *retval = NULL;
  const char *cur;
  for(cur = s; *cur; cur++)
    if(*cur == c)
      retval = (char*)cur;
  return retval;
}

char *strstr(const char *str1, const char *str2) {
  size_t len_str2 = strlen(str2);
  char *cur;

  for (cur = (char*)str1; cur != NULL; cur = strchr(cur, *str2)) {
    if (!strncmp(cur, str2, len_str2)) {
      break;
    }
    cur++;
  }

  return cur;
}

void   bzero(void *ptr, size_t size)
{
  unsigned long i;
  unsigned char *p = ptr;
  for(i = 0; i < size; ++i) p[i] = 0;
}



