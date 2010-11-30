# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

HALVM_SRCS=ARPSniffer.hs
TEST_VMS=ARPSniffer
TEST_SUBJECTS=ARPSniffer
VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

runX: ARPSniffer
	-@$(XM) destroy ARPSniffer
	$(XM) create ARPSniffer.config -c

