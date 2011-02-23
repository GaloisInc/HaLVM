// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
//
#include <stdlib.h>
#include <string.h>
#include <xenctrl.h>
#include "libIVC.h"
#include "ivc_private.h"
#include <arpa/inet.h>

struct unidir_chan {
  struct channel_core core;
  unsigned long otherDom;
};

static struct unidir_chan *new_chan(char *name)
{
  struct unidir_chan *res = malloc(sizeof(struct unidir_chan));
  int test = bind_memory_and_port(name, &res->otherDom, &res->core);
  if(!test) {
    free(res);
    return NULL;
  }

  return res;
}

in_chan *connect_one_way_in(char *name)
{
  return (in_chan*)new_chan(name);
}

out_chan *connect_one_way_out(char *name)
{
  return (out_chan*)new_chan(name);
}

unsigned long inchannel_peer(in_chan *chan)
{
  return chan->otherDom;
}

unsigned long outchannel_peer(out_chan *chan)
{
  return chan->otherDom;
}

int read_inchan(in_chan *chan, void *buffer, int size)
{
  unsigned long len = pull_next_size(&chan->core);

  if(len > size)
    return 0;

  return internal_read(&chan->core, buffer, len);
}

int read_unknown_inchan(in_chan *chan, void **out_buffer)
{
  unsigned long len = pull_next_size(&chan->core);

  *out_buffer = malloc(len);
  return internal_read(&chan->core, *out_buffer, len);
}

int write_outchan(out_chan *chan, void *buffer, int size)
{
  unsigned long write_size;

  if(size <= 0)
    return 0;
  
  write_size = htonl(size);
  if(internal_write(&chan->core, &write_size, 4) != 4)
    return 0;
 

  return internal_write(&chan->core, buffer, size);
}
