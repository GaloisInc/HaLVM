// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
//
#ifndef HALVM_IVC_PRIVATE_H
#define HALVM_IVC_PRIVATE_H

#include <xenctrl.h>
#include <xs.h>
#include <stdint.h>

extern struct xs_handle *xsd;

struct ivc_shared_page {
  volatile uint32_t bytes_consumed;
  volatile uint32_t bytes_produced;
} __packed__;

typedef struct ivc_shared_page ivc_shared_page;

struct channel_core {
  char *mem;
  struct ivc_shared_page *block;
  // the size of the available space.  the actual allocated space is
  // ring_size + sizeof(ivc_shared_page)
  int ring_size;
  evtchn_port_t port;
  EVTCHN_INTERFACE_TYPE xce;
};

void initialize_libIVC_library(void);
int resize_channel_core(struct channel_core *chan, unsigned int size, char **mem);
int bind_memory_and_port(char *key, unsigned long *other_dom,
                         struct channel_core *chan);
void push_next_size(struct channel_core *chan, uint32_t size);
uint32_t pull_next_size(struct channel_core *chan);
void internal_read(struct channel_core *chan, void *buffer, int size);
void internal_write(struct channel_core *chan, void *buffer, int size);

#endif
