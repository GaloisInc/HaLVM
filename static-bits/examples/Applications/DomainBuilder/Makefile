# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND
default: test

NO_HARNESS=y
HALVM_SRCS=testIvc.hs testminios.hs testlinux.hs testConsole.hs Hello.hs \
	   ConsoleChild.hs
TEST_VMS=Hello ConsoleChild testIvc testminios testlinux testConsole
TEST_SUBJECTS=Hello ConsoleChild testIvc testminios testlinux testConsole \
	      ramdisk.cpio.gz init

TOP=$(CURDIR)/../..
include $(TOP)/mk/rules.mk
include $(TOP)/mk/tests.mk

HC_OPTS+=-O

# Issue: these definitions really require XEN_BIN_ROOT to point into
# the source distribution.
MINIOS = $(XEN_BIN_ROOT)/../../extras/mini-os/mini-os	
# LINUX  = $(XEN_BIN_ROOT)/boot/vmlinuz-2.6-xen
LINUX  = $(XEN_BIN_ROOT)/boot/vmlinuz-2.6.*xen*


init: init.c
	gcc -static -Wall init.c -o init

ramdisk.cpio.gz: init
	echo init | cpio -o -H newc | gzip > ramdisk.cpio.gz

testIvc.img: testIvc Hello
testIvc.img: DISK_SIZE=5000000

testminios.img: testminios $(MINIOS)

testlinux.img: testlinux $(LINUX) ramdisk.cpio.gz
testlinux.img: DISK_SIZE=160000000

testConsole.img: testConsole ConsoleChild

test: testIvc.test testminios.test testlinux.test 

# testConsole.test

clean::
	rm -f *.img *~
