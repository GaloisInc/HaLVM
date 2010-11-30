# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND
#

HARNESS_DIR=Harness
HALVM_SRCS=Xenstore.hs XenstoreC.hs
TEST_VMS=Xenstore XenstoreC
TEST_SUBJECTS=Xenstore XenstoreC
TEST_CABAL=XenstoreExamples.cabal
VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

runX: $(TEST_SUBJECTS) tidy
	@$(XM) create Xenstore.config -c

runXC: $(TEST_SUBJECTS) tidy
	@$(XM) create XenstoreC.config -c

