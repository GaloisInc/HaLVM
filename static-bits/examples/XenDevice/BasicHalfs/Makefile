# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick      <awick@galois.com>
#         Trevor Elliott <trevor@galois.com>
# BANNEREND

NO_HARNESS=y
HALVM_SRCS=FSRead.hs FSWrite.hs Utils.hs
TEST_VMS=FSRead FSWrite
TEST_SUBJECTS=FSRead FSWrite
TEST_CABAL=BasicHalfs.cabal
VERBOSE_TEST=--verbose

DISK_FILE=$(shell pwd)/disk.img

build:: FSWrite.config FSRead.config

all: $(TEST_SUBJECTS)

include ../../mk/tests.mk

run: disk_file FSWrite.config FSRead.config
	@$(XM) create FSWrite.config -c
	@$(XM) create FSRead.config -c

FSWrite.config: FSWrite.config.in
	cp FSWrite.config.in FSWrite.config
	echo "disk    = ['file:$(DISK_FILE),ioemu:hda1,w']" >> FSWrite.config

FSRead.config: FSRead.config.in
	cp FSRead.config.in FSRead.config
	echo "disk    = ['file:$(DISK_FILE),ioemu:hda1,w']" >> FSRead.config

disk_file:
	$(RM) $(DISK_FILE)
	dd if=/dev/zero of=$(DISK_FILE) bs=1M count=64
	mkhalfs -d $(DISK_FILE) -l 1024

clean::
	$(RM) {FSWrite,FSRead}.config disk.img

