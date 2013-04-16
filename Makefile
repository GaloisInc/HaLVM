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
AUTORECONF ?= autoreconf

.PHONY: all
all:

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
	$(RM) -f configure mk/autoconf.mk .submodule.init
	$(RM) -rf halvm-ghc/libraries/base

#############################################################################
#
# Basic Tree and Makefile contruction
#
#############################################################################

quiet_cmd_ghcboot     = BOOT        ghc
      cmd_ghcboot     = (cd halvm-ghc && ./boot)
halvm-ghc/configure: .submodule.init halvm-ghc/configure.ac halvm-ghc/boot
	$(call cmd,ghcboot)

SYNC_ALL_FLAGS            = --no-dph
SYNC_ALL_FLAGS           += -r
SYNC_ALL_FLAGS           += http://darcs.haskell.org/

quiet_cmd_syncall    = SYNC_ALL    ghc-libraries
cmd_syncall    = (cd halvm-ghc && ./sync-all $(SYNC_ALL_FLAGS) get && rm -rf libraries/base && $(GIT) clone $(GIT_LIBRARIES_URL)/halvm-base.git --branch halvm libraries/base)
halvm-ghc/libraries/base/base.cabal: .submodule.init
	$(call cmd,syncall)

HALVM_GHC_CONFIGURE_FLAGS  = --target=$(TARGET_ARCH)
HALVM_GHC_CONFIGURE_FLAGS += --with-gcc=$(CC)
HALVM_GHC_CONFIGURE_FLAGS += --with-ld=$(LD)
HALVM_GHC_CONFIGURE_FLAGS += --with-nm=$(NM)
HALVM_GHC_CONFIGURE_FLAGS += --with-objdump=$(OBJDUMP)

halvm-ghc/mk/config.mk: CONFIGURE_FLAGS=$(HALVM_GHC_CONFIGURE_FLAGS)
halvm-ghc/mk/config.mk: mk/autoconf.mk .submodule.init halvm-ghc/configure
halvm-ghc/mk/config.mk: halvm-ghc/libraries/base/base.cabal
	$(call cmd,configure)

BUILD_MK_REWRITES          = -DC_FLAGS="$(CFLAGS)"
BUILD_MK_REWRITES         += -DGHC_FLAGS="$(GHCFLAGS)"
BUILD_MK_REWRITES         += -DINT_LIBRARY=$(INTEGER_LIBRARY)

quiet_cmd_cpp        = CPP        $@
      cmd_cpp        = $(CPP) $(CPP_FLAGS) -P -x c $(lastword $^) > $@
halvm-ghc/mk/build.mk: CPP_FLAGS=$(BUILD_MK_REWRITES)
halvm-ghc/mk/build.mk: .submodule.init mk/build.mk.in
	$(call cmd,cpp)

clean::
	$(RM) -f halvm-ghc/mk/build.mk halvm-ghc/mk/config.mk

mrproper::
	$(RM) -f halvm-ghc/configure

all: halvm-ghc/mk/build.mk halvm-ghc/libraries/base/base.cabal
