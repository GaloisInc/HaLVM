// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Authors: Adam Wick <awick@galois.com>, Rebekah Leslie <rebekah@galois.com>
// BANNEREND
#ifndef XEN_STRING_H
#define XEN_STRING_H

#include <types.h>

void  *memcpy(void *dest, const void *src, size_t count);
void  *memmove(void *dest, const void *src, size_t n);
int    memcmp(const void *s1, const void *s2, size_t n);
void  *memchr(const void *s, int c, size_t n);
int    strcmp(const char *cs, const char *ct);
int    strncmp(const char *str1, const char *str2, size_t count);
char  *strncpy(char *dest, const char *src, size_t count);
void  *memset(void *s,int c, size_t count);
size_t strnlen(const char *s, size_t count);
size_t strlen(const char *s);
char  *strerror(int errnum);
char  *strchr(const char *s, int c);
char  *strrchr(const char *s, int c);
char  *strstr(const char *str1, const char *str2);
char  *strcpy(char *dest, const char *src);

#endif
