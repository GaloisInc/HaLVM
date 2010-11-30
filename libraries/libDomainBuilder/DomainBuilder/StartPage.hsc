-- BANNERSTART
-- - Copyright 2006-2008, Galois, Inc.
-- - This software is distributed under a standard, three-clause BSD license.
-- - Please see the file LICENSE, distributed with this software, for specific
-- - terms and conditions.
-- Author: Paul Graunke <pgraunke@galois.com>
-- BANNEREND
module DomainBuilder.StartPage(nr_pages, shared_info, pt_base, nr_pt_frames, mfn_list, cmd_line, mod_start, mod_len, max_guest_cmdline, store_mfn, store_evtchn, console_domU_mfn, console_domU_evtchn)
where

import DomainBuilder.SafePage (Offset)

#include <xen/xen.h>

nr_pages :: Offset
nr_pages = (#offset start_info_t, nr_pages)

shared_info :: Offset
shared_info = (#offset start_info_t, shared_info)

pt_base :: Offset
pt_base = (#offset start_info_t, pt_base)

nr_pt_frames :: Offset
nr_pt_frames = (#offset start_info_t, nr_pt_frames)

mfn_list :: Offset
mfn_list = (#offset start_info_t, mfn_list)

mod_start :: Offset
mod_start = (#offset start_info_t, mod_start)

mod_len :: Offset
mod_len = (#offset start_info_t, mod_len)

cmd_line :: Offset
cmd_line = (#offset start_info_t, cmd_line)

max_guest_cmdline :: Integer
max_guest_cmdline = (#const MAX_GUEST_CMDLINE)

store_mfn :: Offset
store_mfn = (#offset start_info_t, store_mfn)

store_evtchn :: Offset
store_evtchn = (#offset start_info_t, store_evtchn)

console_domU_mfn :: Offset
console_domU_mfn = (#offset start_info_t, console.domU.mfn)

console_domU_evtchn :: Offset
console_domU_evtchn = (#offset start_info_t, console.domU.evtchn)
