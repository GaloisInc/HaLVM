# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com> and Adam Wick <awick@galois.com>
# BANNEREND
#
BINARIES=Start Middle End
include ../../standard.mk

run2: $(BINARIES)
	-sudo xl destroy Start
	-sudo xl destroy End
	sudo xl create Start.config    "name='Start'" "extra='outchan=SpeedTest2'"
	sudo xl create End.config   -c "name='End'"   "extra='inchan=SpeedTest2'"

stop:
	-sudo xl destroy Start
	-sudo xl destroy End
