# BANNERSTART
# - Copyright 2013, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
BINARIES=logger Server
include ../../standard.mk

run: $(BINARIES)
	-sudo xl destroy Server
	-sudo killall logger
	sudo ./logger > output &
	sudo xl create Server.config
	sleep 3
	sudo xl dmesg -c
	sudo cat output
	sudo rm output
