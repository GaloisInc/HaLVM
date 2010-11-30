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
TEST_CONFIGS=$(foreach vm,$(TEST_VMS),$(vm).config)
HARNESS_DIR=Harness
TEST_CABAL=CopyTest.cabal

# If testing JEH's XSM work, uncomment the following line:
# XSM=xsm
# and comment out this one:
XSM=no-xsm

VERBOSE_TEST=--verbose

all: $(TEST_CONFIGS) test

%.config: %.config.$(XSM)
	$(CP) $< $@

include ../../mk/tests.mk

runX: $(TEST_CONFIGS) $(TEST_SUBJECTS) tidy
	$(XM) dmesg --clear > /dev/null
	$(XM) create Receiver.config
	$(XM) create Sender.config
	@sleep 3
	$(XM) dmesg

clean::
	-for CONFIG in $(TEST_CONFIGS); do $(RM) $$CONFIG; done

