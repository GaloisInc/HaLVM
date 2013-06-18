# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
BINARIES=VBDTest
include ../../standard.mk

disk.img:
	dd if=/dev/zero of=$@ bs=1M count=64

clean::
	rm -f disk.img
