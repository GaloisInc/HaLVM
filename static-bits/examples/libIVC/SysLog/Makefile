# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

NO_HARNESS=y
HALVM_SRCS=TestLogger.hs
CSRCS=$(CSRCDIR)/halvm_logger.c
CEXE=./halvm_logger
TEST_SUBJECTS=TestLogger $(CEXE)
TEST_VMS=TestLogger

VERBOSE_TEST=--verbose

all: $(TEST_SUBJECTS)

include ../../mk/tests.mk

run: $(TEST_SUBJECTS)
	@$(MAKE) --no-print-directory runC &
	@$(XM) create TestLogger.config -c

runC: $(CEXE)
	$(SUDO) $(CEXE)

