# Copyright 2006-2016, Galois, Inc.
# This software is distributed under a standard, three-clause BSD license.
# Please see the file LICENSE, distributed with this software, for specific
# terms and conditions.

.PHONY: all clean

all::
clean::

include autoconf.mk

################################################################################

MINIOS_OBJ:=$(TOPDIR)/mini-os/mini-os.o

all:: $(MINIOS_OBJ)

clean::
	$(MAKE) -C mini-os clean

$(MINIOS_OBJ): $(shell find mini-os/ -name '*.[cSh]')
	$(MAKE) -C mini-os

################################################################################

SOLO5_OBJ:=$(TOPDIR)/solo5/kernel/virtio/solo5.o

all:: $(SOLO5_OBJ)

clean::
	$(MAKE) -C solo5 clean

$(SOLO5_OBJ): $(shell find solo5/ -name '*.[cSh]')
	$(MAKE) -C solo5 virtio

################################################################################



################################################################################

HALVM_GHC_CONFIGURE_FLAGS  = --target=$(ARCH)-unknown-HaLVM
HALVM_GHC_CONFIGURE_FLAGS += --with-gcc=$(CC)
HALVM_GHC_CONFIGURE_FLAGS += --with-ld=$(LD)
HALVM_GHC_CONFIGURE_FLAGS += --with-nm=$(NM)
HALVM_GHC_CONFIGURE_FLAGS += --with-ar=$(AR)
HALVM_GHC_CONFIGURE_FLAGS += --with-objdump=$(OBJDUMP)
HALVM_GHC_CONFIGURE_FLAGS += --with-ranlib=$(RANLIB)
HALVM_GHC_CONFIGURE_FLAGS += --with-readelf=$(READELF)
HALVM_GHC_CONFIGURE_FLAGS += --with-ghc=$(GHC)
HALVM_GHC_CONFIGURE_FLAGS += --prefix=$(prefix)

$(TOPDIR)/halvm-ghc/mk/build.mk: $(TOPDIR)/misc/build.mk
	$(LN) -sf $(TOPDIR)/misc/build.mk $@

$(TOPDIR)/halvm-ghc/libraries/base/ghc.mk: $(TOPDIR)/halvm-ghc/mk/build.mk
	(cd halvm-ghc && ./boot)

$(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/base/ghc.mk
	(cd halvm-ghc && RELEASE=YES ./configure $(HALVM_GHC_CONFIGURE_FLAGS))

$(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1: $(TOPDIR)/halvm-ghc/mk/config.mk
	$(MAKE) -C halvm-ghc ghclibdir=$(halvmlibdir)

clean::
	$(RM) -f $(TOPDIR)/halvm-ghc/mk/config.mk
	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/base.ghc.mk
	$(MAKE) -c halvm-ghc clean

