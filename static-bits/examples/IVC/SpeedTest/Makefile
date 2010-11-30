# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com> and Adam Wick <awick@galois.com>
# BANNEREND
#

#NO_HARNESS=y
HARNESS_DIR=Harness
HALVM_SRCS=ST_Start.hs ST_Middle.hs ST_End.hs Common.hs
TEST_VMS=Start2 End2 \
         Start3 Middle3 End3 \
         Start6 Middle6_1 Middle6_2 Middle6_3 Middle6_4 End6
MULTI_STAGE=Start Middle End
TEST_SUBJECTS=$(foreach vm,Start Middle End,dist/build/$(vm)/$(vm))
TEST_CABAL=SpeedTest.cabal

all: $(TEST_SUBJECTS)

include ../../mk/tests.mk

# These all share the same .cabal file, so there's no makefile-relevant
# distinction (wrt to need for recompilation) between testing two, three,
# or six VMs, so we just use $(TEST_SUBJECTS) as the dependency list.

runX: $(TEST_SUBJECTS)
	@$(MAKE) --no-print-directory run2
	@$(MAKE) --no-print-directory run3
	@$(MAKE) --no-print-directory run6
	
run2: $(TEST_SUBJECTS)
	@-$(XM) destroy Start2 > /dev/null 2>&1
	@-$(XM) destroy End2 > /dev/null 2>&1
	@$(XM) create Start.config name="Start2" extra="outchan=SpeedTest2"
	@$(XM) create End.config name="End2" extra="inchan=SpeedTest2" -c

run3: $(TEST_SUBJECTS)
	@-$(XM) destroy Start3 > /dev/null 2>&1
	@-$(XM) destroy Middle3 > /dev/null 2>&1
	@-$(XM) destroy End3 > /dev/null 2>&1
	@$(XM) create Start.config name="Start3" extra="outchan=SpeedTest3_1"
	@$(XM) create Middle.config name="Middle3" \
	  extra="inchan=SpeedTest3_1 outchan=SpeedTest3_2"
	@$(XM) create End.config name="End3" extra="inchan=SpeedTest3_2" -c

run6: $(TEST_SUBJECTS)
	@-$(XM) destroy Start6 > /dev/null 2>&1
	@-$(XM) destroy Middle6_1 > /dev/null 2>&1
	@-$(XM) destroy Middle6_2 > /dev/null 2>&1
	@-$(XM) destroy Middle6_3 > /dev/null 2>&1
	@-$(XM) destroy Middle6_4 > /dev/null 2>&1
	@-$(XM) destroy End6 > /dev/null 2>&1
	@$(XM) create Start.config name="Start6" extra="outchan=SpeedTest6_1"
	@$(XM) create Middle.config name="Middle6_1" \
	  extra="inchan=SpeedTest6_1 outchan=SpeedTest6_2"
	@$(XM) create Middle.config name="Middle6_2" \
	  extra="inchan=SpeedTest6_2 outchan=SpeedTest6_3"
	@$(XM) create Middle.config name="Middle6_3" \
	  extra="inchan=SpeedTest6_3 outchan=SpeedTest6_4"
	@$(XM) create Middle.config name="Middle6_4" \
	  extra="inchan=SpeedTest6_4 outchan=SpeedTest6_5"
	@$(XM) create End.config name="End6" extra="inchan=SpeedTest6_5" -c

