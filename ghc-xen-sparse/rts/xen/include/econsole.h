// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef XEN_DOM_ECONSOLE_H
#define XEN_DOM_ECONSOLE_H

void emergency_console_msg(int len,char *msg);
int emergency_console_read(char *buf, int size);

#endif
