# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

include mk/common.mk
-include mk/autoconf.mk

.PHONY: all
all::

.PHONY: clean
clean::

.PHONY: mrproper
mrproper:: clean

#############################################################################
#
# Basic Tree and Makefile contruction
#
#############################################################################

quiet_cmd_gitsubmod  = GITSUB      $@
      cmd_gitsubmod  = $(GIT) submodule update --init --recursive --quiet && \
                      touch $@
.submodule.init:
	$(call cmd,gitsubmod)

quiet_cmd_autoreconf = AUTOCONF    $@
      cmd_autoreconf = (cd $(dir $(lastword $^)) && $(AUTORECONF))
./configure: configure.ac
	$(call cmd,autoreconf)

quiet_cmd_configure  = CONFIGURE   $@
      cmd_configure  = (cd $(dir $(lastword $^)) && \
                        ./configure $(CONFIGURE_FLAGS))
mk/autoconf.mk: mk/autoconf.mk.in ./configure
	$(call cmd,configure)

mrproper::
	$(RM) -f configure mk/autoconf.mk

#all: halvm-ghc/.submodule.init
#	$(MAKE) -f mk/infrastructure.mk all
#	$(MAKE) -f mk/halvm.mk all
#
#docs:
#	$(MAKE) -f mk/infrastructure.mk all
#	$(MAKE) -f mk/halvm.mk docs
#
#infrastructure:
#	$(MAKE) -f mk/infrastructure.mk all
#	$(MAKE) -f mk/tests.mk all
#
#examples:
#	$(MAKE) -f mk/tests.mk examples
#
#tests: infrastructure
#	$(MAKE) -f mk/tests.mk tests
#
#clean:
#	$(MAKE) -f mk/halvm.mk clean
#	$(MAKE) -f mk/infrastructure.mk clean
#
#partial-clean:
#	$(MAKE) -f mk/halvm.mk partial-clean
#
#mrproper:
#	$(MAKE) -f mk/halvm.mk mrproper
#	$(MAKE) -f mk/infrastructure.mk mrproper
#	$(MAKE) -f mk/tests.mk clean
#
#remove-%:
#	$(MAKE) -f mk/halvm.mk $@
#
#install:
#	$(MAKE) -f mk/halvm.mk install
