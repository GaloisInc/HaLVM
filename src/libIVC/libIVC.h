// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef HALVM_LIBIVC_H
#define HALVM_LIBIVC_H

#include <stdint.h>

enum ivc_contype
{
  ivcInputChannel,
  ivcOutputChannel,
  ivcInputOutputChannel
};

typedef struct libIVC_interface libIVC_t;
typedef struct ivc_connection   ivc_connection_t;
typedef enum   ivc_contype      ivc_contype_t;

libIVC_t *openIVCLibrary(void);
void      closeIVCLibrary(libIVC_t *);

ivc_connection_t *makeConnection(libIVC_t *, char *, ivc_contype_t, float);
ivc_connection_t *acceptConnection(libIVC_t *, char *, ivc_contype_t,
                                   uint32_t, float);
void closeConnection(libIVC_t*, ivc_connection_t*);

void *getData(ivc_connection_t*);
void putData(ivc_connection_t*, void *, size_t);
uint32_t connectionPeer(ivc_connection_t*);

#endif
