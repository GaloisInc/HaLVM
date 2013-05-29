# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

HALVM_SRCS=Hello.hs
TEST_SUBJECTS=Hello
TEST_VMS=Hello

all: test

include ../../mk/tests.mk

run: $(TEST_SUBJECTS)
	@$(XM) dmesg -c > /dev/null
	@$(XM) create ${TEST_VMS}.config -c
	@$(XM) dmesg

