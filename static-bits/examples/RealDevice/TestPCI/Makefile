# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Iavor Diatchki <diatchki@galois.com>
# BANNEREND

HALVM_SRCS=Main.hs Resources.hs PCIRegs.hs ATH9k.hs BAR.hs Capabilities.hs
TEST_SUBJECTS=Main
TEST_VMS=Main
#VERBOSE_TEST=--verbose

all: test

include ../../mk/tests.mk

DISK_SIZE=8000000
QEMU_FLAGS=-serial stdio

all: example

