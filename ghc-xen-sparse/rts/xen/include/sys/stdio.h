// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_HOST_STDIO_H
#define XEN_HOST_STDIO_H

// libgmp checks for this to determine if it should be using FILE
// and when it does not, things go bad.
#ifndef H_STDIO
#define H_STDIO
#endif


#include <stdarg.h>
#include <types.h>

#define EOF (-1)

typedef unsigned long FILE;

#define _p(_x) ((void *)(unsigned long)(_x))
int printf(const char *fmt, ...);
int fprintf(FILE *stream, const char *format, ...);
int vsnprintf(char *buf, size_t size, const char *fmt, va_list args);
int snprintf(char *buf, size_t size, const char *fmt, ...);
int sprintf(char * buf, const char *fmt, ...);
int vfprintf(FILE *stream, const char *format, va_list ap);

int puts(const char *s);
int fputs(const char *s, FILE *stream);
int fputc(int c, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

int fflush(FILE *stream);
FILE *fopen(const char *path, const char *mode);
int fclose(FILE *fp);

void perror(const char *s);

extern FILE *stdout;
extern FILE *stdin;
extern FILE *stderr;

#endif
