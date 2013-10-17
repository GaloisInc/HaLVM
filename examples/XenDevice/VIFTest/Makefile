# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
BINARIES=VIFTest
include ../../standard.mk

run: VIFTest
	-sudo xl destroy VIFTest
	sudo xl create VIFTest.config -c
