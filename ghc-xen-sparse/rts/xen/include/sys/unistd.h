// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_UNISTD_H
#define XEN_UNISTD_H

#include <sys/types.h>

long sysconf(int name);

#define _SC_CLK_TCK             2
#define _SC_PAGESIZE            30

int unlink(const char *pathname);
int access(const char *pathname, int mode);
int ftruncate(int fd, off_t length);

struct statfs {};

/* should be in sys/vfs.h, adam -te */
int statfs(const char *path, struct statfs *buf);

int close(int fd);

size_t getline(char **lineptr __attribute__ ((unused)),
    size_t *n __attribute__ ((unused)),
    FILE *stream __attribute__ ((unused)));

size_t __getdelim(char **lineptr __attribute__ ((unused)),
    size_t *n __attribute__ ((unused)),
    int delim __attribute__ ((unused)),
    FILE *stream __attribute__ ((unused)));

#endif
