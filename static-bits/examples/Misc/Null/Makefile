# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

NO_HARNESS=y

HALVM_SRCS=Null.hs
TEST_SUBJECTS=Null
TEST_VMS=Null

all: $(TEST_SUBJECTS)

include ../../mk/tests.mk

run: $(TEST_SUBJECTS)
	@$(XM) dmesg -c > /dev/null
	@$(XM) create Null.config
	@$(XM) dmesg > $(TEST_RESULTS)

