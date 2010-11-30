# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND

NO_HARNESS=y
HALVM_SRCS=ROT13Test.hs
CSRCS=$(CSRCDIR)/rot13enc.c
CEXE=./rot13enc
TEST_SUBJECTS=ROT13Test $(CEXE)
TEST_VMS=ROT13Test

VERBOSE_TEST=--verbose

all: $(TEST_SUBJECTS)

include ../../mk/tests.mk

run: $(TEST_SUBJECTS)
	@$(MAKE) --no-print-directory runC &
	@$(XM) create ROT13Test.config -c

runC: $(CEXE)
	$(SUDO) $(CEXE)

