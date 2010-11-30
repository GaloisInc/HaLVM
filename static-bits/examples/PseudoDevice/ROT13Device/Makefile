# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND

HARNESS_DIR=Harness
HALVM_SRCS=Backend.hs Test.hs ROT13.hs ROT13Ring.hs
TEST_VMS=Backend Test
TEST_SUBJECTS=Backend Test
TEST_CABAL=ROT13Device.cabal
VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

runBackend: Backend
	@-$(XM) destroy Backend
	@$(XM) create Backend.config -c

runTest: Test
	@-$(XM) destroy Test
	@$(XM) create Test.config -c

