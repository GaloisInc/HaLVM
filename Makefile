# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

include mk/common.mk
include mk/autoconf.mk

.PHONY: all
all:

.PHONY: clean
clean::

.PHONY: mrproper
mrproper:: clean

.PHONY: install
install:

include libm/build.mk

#############################################################################
#
# Basic Tree and Makefile contruction
#
#############################################################################

mrproper::
	$(RM) -f configure mk/autoconf.mk
	$(RM) -rf halvm-ghc/libraries/base

#############################################################################
#
# Basic Tree and Makefile contruction
#
#############################################################################

SYNC_ALL_FLAGS            = --no-dph
SYNC_ALL_FLAGS           += -r
SYNC_ALL_FLAGS           += http://darcs.haskell.org/

halvm-ghc/libraries/base/base.cabal:
	$(call label,halvm-ghc/sync-all)(cd halvm-ghc \
	  && ./sync-all $(SYNC_ALL_FLAGS) get \
	  && rm -rf libraries/base \
	  && $(GIT) clone $(GIT_LIBRARIES_URL)/halvm-base.git --branch halvm libraries/base)

clean::
	$(RM) -f halvm-ghc/mk/build.mk halvm-ghc/mk/config.mk

mrproper::
	$(RM) -f halvm-ghc/configure

all: halvm-ghc/mk/build.mk halvm-ghc/libraries/base/base.cabal


# Build GHC ####################################################################

halvm-ghc/configure: halvm-ghc/configure.ac halvm-ghc/boot
	$(call label,halvm-ghc/boot)(cd halvm-ghc && ./boot)

HALVM_GHC_CONFIGURE_FLAGS  = --target=$(TARGET_ARCH)
HALVM_GHC_CONFIGURE_FLAGS += --with-gcc=$(CC)
HALVM_GHC_CONFIGURE_FLAGS += --with-ld=$(LD)
HALVM_GHC_CONFIGURE_FLAGS += --with-nm=$(NM)
HALVM_GHC_CONFIGURE_FLAGS += --with-objdump=$(OBJDUMP)
HALVM_GHC_CONFIGURE_FLAGS += --prefix=$(halvm-prefix)

halvm-ghc/mk/config.mk: mk/autoconf.mk halvm-ghc/configure
halvm-ghc/mk/config.mk: halvm-ghc/libraries/base/base.cabal
	$(call label,CONFIGURE GHC)(cd halvm-ghc && \
		./configure $(HALVM_GHC_CONFIGURE_FLAGS) )

halvm-ghc/mk/build.mk: mk/build.mk
	$(call cmd,cp)

all: halvm-ghc/inplace/bin/ghc-stage1

halvm-ghc/inplace/bin/ghc-stage1: halvm-ghc/mk/config.mk
	$(MAKE) -C halvm-ghc

clean::
	$(MAKE) -C halvm-ghc clean

mrproper::
	$(RM) static-bits/bin/halvm-cabal
	$(RM) static-bits/bin/halvm-config
	$(RM) static-bits/bin/halvm-ghc
	$(RM) static-bits/bin/halvm-ghc-pkg


# Installation #################################################################

quiet_cmd_install = INSTALL   $@
      cmd_install = install -D $< $@

programs := $(bindir)/halvm-ghc $(bindir)/halvm-ghc-pkg  \
            $(bindir)/halvm-cabal $(bindir)/halvm-config \
            $(bindir)/make_halvm_dir.py

$(programs): $(bindir)/%: $(TOPDIR)/static-bits/bin/%
	$(call cmd,install)

.PHONY: install-ghc
install-ghc:
	$(call label,halvm-ghc install)$(MAKE) -C halvm-ghc install

# linker script
$(halvm-dir)/ldkernel: static-bits/lib/ldkernel
	$(call cmd,install)

$(halvm-dir)/kernel.lds: static-bits/lib/kernel-$(ARCH).lds
	$(call cmd,install)


install: $(programs) $(halvm-dir)/ldkernel $(halvm-dir)/kernel.lds
install: | install-ghc
