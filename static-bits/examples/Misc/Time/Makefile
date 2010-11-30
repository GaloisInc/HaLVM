# BANNERSTART
# - Copyright 2009, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

HARNESS_DIR=Harness
HALVM_SRCS=Main.hs
TEST_SUBJECTS=TimeTest
TEST_VMS=TimeTest
TEST_CABAL=Time.cabal

all: test

include ../../mk/tests.mk

runX: $(TEST_SUBJECTS)
	-sudo xm destroy TimeTest
	sudo xm create TimeTest.config -c

