// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_ERRNO_H
#define XEN_ERRNO_H

#define EPERM           1
#define ENOENT          2
#define ESRCH           3
#define EIO             5
#define EBADF           9
#define EAGAIN          11
#define ENOMEM          12
#define EACCES          13
#define EFAULT          14
#define EBUSY           16
#define EEXIST          17
#define EISDIR          21
#define EINVAL          22
#define ENOSPC          28
#define EROFS           30
#define ERANGE          34
#define EDEADLK         35
#define ENOTEMPTY       39
#define ENOSYS          78
#define EISCONN         106

extern int errno;
int *__errno_location(void);

#endif
