// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#include <fcntl.h>
#include <errno.h>

int fcntl(int fd __attribute__((unused)), 
	  int cmd __attribute__((unused)), ...)
{
  errno = EACCES; // Seems general enough.
  return -1;
}
