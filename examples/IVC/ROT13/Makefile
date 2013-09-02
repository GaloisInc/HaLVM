# BANNERSTART
# - Copyright 2013, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
BINARIES=rot13 Terminal
include ../../standard.mk

run: $(BINARIES)
	-sudo xl destroy Terminal
	-sudo killall rot13
	sudo ./rot13 &
	sudo xl create Terminal.config -c


