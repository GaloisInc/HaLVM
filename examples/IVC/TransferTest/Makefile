# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#
BINARIES=Receiver Sender
include ../../standard.mk

run: $(BINARIES)
	-sudo xl destroy Receiver
	-sudo xl destroy Sender
	sudo xl create Receiver.config
	sudo xl create Sender.config
	sleep 3
	sudo xl dmesg -c
