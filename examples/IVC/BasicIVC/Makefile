# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#
BINARIES=Peer1 Peer2
include ../../standard.mk

run: Peer1 Peer2
	-sudo xl destroy Peer1
	-sudo xl destroy Peer2
	sudo xl create Peer1.config
	sudo xl create Peer2.config
	sleep 3
	sudo xl dmesg -c
