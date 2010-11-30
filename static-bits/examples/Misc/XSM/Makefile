# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND
TOP=$(CURDIR)/../..
include $(TOP)/mk/rules.mk

POLICY_DIR=$(XEN_BIN_ROOT)/../../tools/flask/policy

all: consoleTest

consoleTest:
	halvm-ghc -package XenDevice consoleTest.hs -o consoleTest

disk.img: grublines $(XEN_BIN_ROOT)/boot/xen.gz consoleTest $(POLICY_DIR)/policy.20
	$(MAKEDISK) disk.img 5000000 $^

run: disk.img
	qemu -nographic -no-kqemu -hda disk.img

test: disk.img
	$(RUNTEST) disk.img testInteractions

clean:
	rm -rf mnt disk.img consoleTest *.hi *.o
