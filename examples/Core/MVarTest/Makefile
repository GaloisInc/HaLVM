# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
BINARIES=MVarTest
include ../../standard.mk

run: MVarTest
	sudo xl create MVarTest.config
	sleep 5
	sudo xl dmesg -c
