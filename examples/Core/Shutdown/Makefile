# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#
BINARIES=Shutdown
include ../../standard.mk

run: Shutdown
	-sudo xl destroy Shutdown
	(sleep 15 && sudo xl shutdown Shutdown) &
	sudo xl create Shutdown.config -c
