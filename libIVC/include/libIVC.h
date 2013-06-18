// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
#ifndef HALVM_LIBIVC_H
#define HALVM_LIBIVC_H

#include <xenctrl.h>
#include <stdlib.h>

// #ifdef XENCTRL_HAS_XC_INTERFACE
// #define XC_HANDLE_TYPE xc_gnttab *
// #define EVTCHN_INTERFACE_TYPE xc_evtchn *
// #define XC_OPEN_ARGS NULL, XC_OPENFLAG_NON_REENTRANT
// #else
// #define XC_HANDLE_TYPE int
// #define EVTCHN_INTERFACE_TYPE int
// #define XC_OPEN_ARGS
// #endif

typedef struct unidir_chan in_chan;
typedef struct unidir_chan out_chan;
typedef struct bidir_chan inout_chan;
typedef struct xbackend xen_backend;

extern xc_evtchn *xce;
extern xc_gnttab *xcg;

void            initialize_libIVC_library(void);
in_chan        *connect_one_way_in(char *name);
out_chan       *connect_one_way_out(char *name);
inout_chan     *connect_two_way(char *name);
// RETURNS: 0 on error, # of bytes read otherwise
int             read_inchan(in_chan *chan, void *buffer, int size);
// RETURNS: 0 on error, # of bytes read otherwise, data in out_buffer
size_t          read_unknown_inchan(in_chan *chan, void **out_buffer);
void            write_outchan(out_chan *chan, void *buffer, int size);
// RETURNS: 0 on error, # of bytes read otherwise
int             read_chan(inout_chan *chan, void *buffer, int size);
// RETURNS: 0 on error, # of bytes read otherwise, data in out_buffer
size_t          read_unknown_chan(inout_chan *chan, void **out_buffer);
void             write_chan(inout_chan *chan, void *buffer, int size);
// Returns the domain id of the other side of the channel.
unsigned long   channel_peer(inout_chan *chan);
unsigned long   inchannel_peer(in_chan *chan);
unsigned long   outchannel_peer(in_chan *chan);
xen_backend    *create_responder(char *name);
void            accept_connection(xen_backend *, unsigned long *,
                                  void **, void* evtchn_port);

#if defined(__i386__)
#define mb()  __asm__ __volatile__ ( "lock; addl $0,0(%%esp)" : : : "memory" )
#define rmb() __asm__ __volatile__ ( "lock; addl $0,0(%%esp)" : : : "memory" )
#define wmb() __asm__ __volatile__ ( "" : : : "memory")
#elif defined(__x86_64__)
#define mb()  __asm__ __volatile__ ( "mfence" : : : "memory")
#define rmb() __asm__ __volatile__ ( "lfence" : : : "memory")
#define wmb() __asm__ __volatile__ ( "" : : : "memory")
#elif defined(__ia64__)
#define mb()   __asm__ __volatile__ ("mf" ::: "memory")
#define rmb()  __asm__ __volatile__ ("mf" ::: "memory")
#define wmb()  __asm__ __volatile__ ("mf" ::: "memory")
#elif defined(__powerpc__)
/* XXX loosen these up later */
#define mb()   __asm__ __volatile__ ("sync" : : : "memory")
#define rmb()  __asm__ __volatile__ ("sync" : : : "memory") /* lwsync? */
#define wmb()  __asm__ __volatile__ ("sync" : : : "memory") /* eieio? */
#else
#error "Define barriers"
#endif

#endif
