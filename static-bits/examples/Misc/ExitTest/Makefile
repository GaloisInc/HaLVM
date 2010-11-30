# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

HALVM_SRCS=ExitTest.hs
TEST_SUBJECTS=ExitTest
TEST_VMS=ExitTest

all: test

include ../../mk/tests.mk

runX: $(TEST_SUBJECTS)
	@$(XM) dmesg -c > /dev/null
	@$(XM) create ExitTest.config -c
	@$(XM) dmesg

