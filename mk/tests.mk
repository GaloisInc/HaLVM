# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND


include mk/common.mk

# NOTE: halfs-utils depends upon halfs, HFuse being installed globally, while
# Testing depends upon orc being installed globally.  In the fullness of time,
# these three libs may end becoming part of infrastructure.mk.

TESTING_TOPDIR      = $(TOPDIR)/static-bits/examples
#TESTING_LIBS        = Testing mkhalfs
TESTING_LIBS        = Testing
TESTING_LIB_LOCS    = $(addprefix $(TOPDIR)/libraries/,$(TESTING_LIBS))
TESTING_LIB_TARGETS = $(addprefix $(TOPDIR)/libraries/.testing-built-,$(TESTING_LIBS))

# We want the system cabal for building the test infrastructure, not
# $(PLATFORM_CABAL_EXE), which is built for building halvm-ghc, etc.
CABAL               = cabal
CABAL_OPTS_mkhalfs  = --bindir=$(TESTING_TOPDIR)/bin

.PHONY: all
all: infrastructure

.PHONY: tests
tests: infrastructure
	$(MAKE) --no-print-directory -C $(TESTING_TOPDIR) tests

infrastructure: $(TESTING_LIB_TARGETS)

$(TOPDIR)/libraries/.testing-built-%:
	cd $(TOPDIR)/libraries/$* && $(CABAL) install $(CABAL_OPTS_$*)
	$(TOUCH) $@

examples:
	$(MAKE) --no-print-directory -C $(TESTING_TOPDIR) examples

clean:
	$(RM) -rf $(TOPDIR)/libraries/.testing-built-*
	for DIR in $(TESTING_LIB_LOCS) ; do				\
	  $(RM) -rf $$DIR/dist ;					\
	done

mrproper: clean
	$(MAKE) --no-print-directory -C $(TESTING_TOPDIR) clean

# Not used yet; needed if the testing infrastructure ever involves multiple
# interdependent targets
define testingtarget
$(TOPDIR)/libraries/.testing-built-$1
endef

