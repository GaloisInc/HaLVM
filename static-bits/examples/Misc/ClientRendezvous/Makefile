# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#
BINARIES=Server Client
include ../../standard.mk

run: Server Client
	-sudo xl destroy Server
	-sudo xl destroy Client1
	-sudo xl destroy Client2
	-sudo xl destroy Client3
	sudo xl create Server.config
	sudo xl create Client.config "name='Client1'"
	sudo xl create Client.config "name='Client2'"
	sudo xl create Client.config "name='Client3'"
	sleep 3
	sudo xl dmesg -c
