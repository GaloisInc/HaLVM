# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
BINARIES=VBDTest
include ../../standard.mk

disk1.img:
	dd if=/dev/zero of=$@ bs=1M count=64

disk2.img: buildRODisk
	./buildRODisk > disk2.img

clean::
	rm -f disk1.img disk2.img

run: disk1.img disk2.img VBDTest
	-sudo xl destroy VBDTest
	sudo xl create VBDTest.config -c
