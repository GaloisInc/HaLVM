// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#include <econsole.h>
#include <hypercall.h>

void emergency_console_msg(int len, char *msg) 
{
	(void)HYPERVISOR_console_io(CONSOLEIO_write, len, msg);
}

int emergency_console_read(char *buf, int size)
{
  return HYPERVISOR_console_io(CONSOLEIO_read, size, buf);
}
