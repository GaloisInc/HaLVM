// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
//
#include <xenctrl.h>
#include "libIVC.h"
#include "ivc_private.h"
#include <arpa/inet.h>
#include <xs.h>
#include <stdlib.h>
#include <stdio.h>

// in this context, inout_chan is bidir_chan
struct bidir_chan {
  struct channel_core wcore;
  struct channel_core rcore;
  unsigned long otherDom;
};

inout_chan *connect_two_way(char *name)
{
  inout_chan *res = malloc(sizeof(inout_chan));
  char *mem = NULL;
  int initSize = 0;
  int size = 0;

  int test = bind_memory_and_port(name, &res->otherDom, &res->rcore);

  if(!test) {
    free(res);
    printf("Unable to bind connection\n");
    return NULL;
  }

  // resize the rcore to half of its current size
  initSize = res->rcore.ring_size + sizeof(ivc_shared_page);
  size     = resize_channel_core(&res->rcore, initSize/2, &mem);

  if(size <= 0) {
    free(res);
    printf("Unable to resize inout_chan->rcore\n");
    return NULL;
  }

  // setup the wcore to use the remaining memory
  res->wcore.mem       = mem;
  res->wcore.ring_size = size - sizeof(ivc_shared_page);
  res->wcore.block     = (ivc_shared_page*)(mem + res->wcore.ring_size);

  return res;
}

unsigned long channel_peer(inout_chan *chan)
{
  return chan->otherDom;
}

int read_chan(inout_chan *chan, void *buffer, int size)
{
  uint32_t len = pull_next_size(&chan->rcore);

  if(len > size) {
    return 0;
  }

  internal_read(&chan->rcore, buffer, len);
  return len;
}

size_t read_unknown_chan(inout_chan *chan, void **out_buffer)
{
  uint32_t len = pull_next_size(&chan->rcore);
  *out_buffer = malloc(len);
  if (*out_buffer == NULL) {
    return 0;
  }
  internal_read(&chan->rcore, *out_buffer, len);
  return len;
}

void write_chan(inout_chan *chan, void *buffer, int size)
{
  push_next_size(&chan->wcore, size);
  internal_write(&chan->wcore, buffer, size);
}
