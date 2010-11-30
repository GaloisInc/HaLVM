# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

HALVM_SRCS=Sender.hs Receiver.hs Common.hs
TEST_SUBJECTS=Sender Receiver
TEST_VMS=Sender Receiver
HARNESS_DIR=Harness
TEST_CABAL=TransferTest.cabal

VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

runX: $(TEST_SUBJECTS) tidy
	@$(XM) dmesg --clear > /dev/null
	@$(XM) create Receiver.config writer=console
	@$(XM) create Sender.config writer=console
	@sleep 3
	@$(XM) dmesg

