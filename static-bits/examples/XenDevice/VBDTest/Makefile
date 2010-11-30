# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND

HALVM_SRCS=VBDTest.hs TestOpts.hs
TEST_VMS=VBDTest
TEST_SUBJECTS=VBDTest $(DISK_FILE)
TEST_CONFIGS=VBDTest.config
VERBOSE_TEST=--verbose

DISK_FILE=$(shell pwd)/disk.img

all: $(TEST_SUBJECTS) test

include ../../mk/tests.mk

$(DISK_FILE):
	dd if=/dev/zero of=$(DISK_FILE) bs=1M count=64

VBDTest.config: VBDTest.config.in
	cp VBDTest.config.in VBDTest.config
	echo "disk    = ['file:$(DISK_FILE),ioemu:hda1,w']" >> VBDTest.config

runX: VBDTest $(DISK_FILE) VBDTest.config
	$(XM) create VBDTest.config -c

clean::
	$(RM) $(DISK_FILE)
