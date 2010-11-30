# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND

NO_HARNESS=y
HALVM_SRCS=CryptoDeviceTest.hs
CSRCS=$(CSRCDIR)/crypto_device.c
CEXE=./crypto_device
TEST_SUBJECTS=CryptoDeviceTest $(CEXE)
TEST_VMS=CryptoDeviceTest

VERBOSE_TEST=--verbose

#all: test
all: $(TEST_SUBJECTS)

include ../../mk/tests.mk

HALVM_CLIBS+=-lcrypto

run: $(TEST_SUBJECTS)
	@$(MAKE) --no-print-directory runC &
	@$(XM) create CryptoDeviceTest.config -c

runC: $(CEXE)
	$(SUDO) $(CEXE)

