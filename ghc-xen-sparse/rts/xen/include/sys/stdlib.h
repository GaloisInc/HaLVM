// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_STDLIB_H
#define XEN_STDLIB_H

#include <types.h>

#ifndef NULL
#define NULL 0
#endif

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

char *getenv(const char *name);
void abort(void);
void pabort(const char *format, ...);
void exit(int status);
int raise(int sig);

void *malloc(size_t size);
void *realloc(void *ptr, size_t size);
void *calloc(size_t nmemb, size_t size);
void free(void *ptr);

double atof(const char *nptr);
long int strtol(const char *nptr, char **endptr, int base);
void *bsearch(const void *, const void *, size_t, size_t, 
              int (*compar)(const void *, const void *));

#define assert(x) if(!(x)) { printf("Assertion failure: file %s, line %d\n", \
                                  __FILE__, __LINE__);                     \
                           do_exit(); }

int mkstemp(char *template);

#endif
