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

run3: $(BINARIES)
	-sudo xl destroy Start
	-sudo xl destroy Middle
	-sudo xl destroy End
	sudo xl create Start.config    "name='Start'" "extra='outchan=SpeedTest2a'"
	sudo xl create Middle.config    "name='Middle'" "extra='inchan=SpeedTest2a outchan=SpeedTest2b'"
	sudo xl create End.config   -c "name='End'"   "extra='inchan=SpeedTest2b'"

run6: $(BINARIES)
	-sudo xl destroy Start
	-sudo xl destroy Middle1
	-sudo xl destroy Middle2
	-sudo xl destroy Middle3
	-sudo xl destroy Middle4
	-sudo xl destroy End
	sudo xl create Start.config    "name='Start'" "extra='outchan=SpeedTest2a'"
	sudo xl create Middle.config    "name='Middle1'" "extra='inchan=SpeedTest2a outchan=SpeedTest2b'"
	sudo xl create Middle.config    "name='Middle2'" "extra='inchan=SpeedTest2b outchan=SpeedTest2c'"
	sudo xl create Middle.config    "name='Middle3'" "extra='inchan=SpeedTest2c outchan=SpeedTest2d'"
	sudo xl create Middle.config    "name='Middle4'" "extra='inchan=SpeedTest2d outchan=SpeedTest2e'"
	sudo xl create End.config   -c "name='End'"   "extra='inchan=SpeedTest2e'"

stop:
	-sudo xl destroy Start
	-sudo xl destroy Middle
	-sudo xl destroy Middle1
	-sudo xl destroy Middle2
	-sudo xl destroy Middle3
	-sudo xl destroy Middle4
	-sudo xl destroy End
