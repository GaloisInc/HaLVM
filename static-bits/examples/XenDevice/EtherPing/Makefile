# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

HALVM_SRCS=Pinger.hs Server.hs Common.hs
TEST_VMS=Pinger Server
TEST_SUBJECTS=Pinger Server
VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

runServer: Server
	@$(XM) create Server.config -c

runPinger: Pinger
	@$(XM) create Pinger.config -c

