# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND

HALVM_SRCS=GetId.hs
TEST_SUBJECTS=GetId
TEST_VMS=GetId

all: test

include ../../mk/tests.mk

runX: $(TEST_SUBJECTS)
	@$(XM) dmesg -c > /dev/null
	@$(XM) create GetId.config -c
	@$(XM) dmesg

