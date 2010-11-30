// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_FCNTL_H
#define XEN_FCNTL_H

int fcntl(int fd, int cmd, ...);

#define O_NONBLOCK 04000
#define F_GETFL 3
#define F_SETFL 4

#endif
