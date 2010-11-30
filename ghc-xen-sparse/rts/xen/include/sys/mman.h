// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_MMAN_H
#define XEN_MMAN_H

#include <types.h>

#define PROT_READ 0x1
#define PROT_WRITE 0x2
#define PROT_EXEC 0x4
#define PROT_NOCACHE 0x8

#define MAP_ANON 0x20
#define MAP_ANONYMOUS 0x20
#define MAP_PRIVATE 0x02

void *mmap(void *start, size_t length, int prot, int flags, 
	   int fd, off_t offset);
int munmap(void *start, size_t length);
void *mremap(void *old_address, size_t old_size, size_t new_size, int flags);

#endif
