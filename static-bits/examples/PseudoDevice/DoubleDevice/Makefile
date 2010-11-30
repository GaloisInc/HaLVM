# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND
#

HARNESS_DIR=Harness
HALVM_SRCS=DoubleDevice.hs DoubleTest.hs DoubleCommon.hs
TEST_VMS=DoubleDevice DoubleTest
TEST_SUBJECTS=DoubleDevice DoubleTest
TEST_CABAL=DoubleDevice.cabal
VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

rundevice: DoubleDevice
	@-$(XM) destroy DoubleDevice
	@$(XM) create DoubleDevice.config -c

runtest: DoubleTest
	@-$(XM) destroy DoubleTest
	@$(XM) create DoubleTest.config -c

