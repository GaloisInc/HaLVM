# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

NO_HARNESS=y
HARNESS_DIR=Harness
HALVM_SRCS=Sender.hs Receiver.hs
TEST_VMS=Sender Receiver
MULTI_STAGE=Sender Receiver
TEST_SUBJECTS=$(foreach vm,Sender Receiver,dist/build/$(vm)/$(vm))
TEST_CABAL=TestMapGrants.cabal

all: $(TEST_SUBJECTS)
include ../../mk/tests.mk

runX: $(TEST_SUBJECTS)
	-$(SUDO) xm destroy Sender
	-$(SUDO) xm destroy Receiver
	$(SUDO) xm dmesg --clear > /dev/null
	$(SUDO) xm create Receiver.config
	$(SUDO) xm create Sender.config
	sleep 3
	$(SUDO) xm dmesg

stop:
	-$(SUDO) xm destroy Sender
	-$(SUDO) xm destroy Receiver

cleanX:
	rm -rf Sender Receiver *.o *.hi dist
	-$(SUDO) xm destroy Sender
	-$(SUDO) xm destroy Receiver
