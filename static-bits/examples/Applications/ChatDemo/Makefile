# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND
#

HALVM_SRCS=ChatDemo.hs ChatDemoBi.hs
TEST_SUBJECTS=ChatDemo ChatDemoBi
TEST_VMS=Server Andy Zebra

VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

runServer: ChatDemo
	$(XM) create ChatDemo.config extra="mode=server" -c

runClient: ChatDemo
	$(XM) create ChatDemo.config extra="mode=client" name=$(NAME) -c

runServerBi: ChatDemoBi
	$(XM) create ChatDemoBi.config extra="mode=server" -c

runClientBi: ChatDemoBi
	$(XM) create ChatDemoBi.config extra="mode=client" name=$(NAME) -c

