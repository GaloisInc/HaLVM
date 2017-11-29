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

MUSL_DEPENDS =
define musl_link
MUSL_DEPENDS += $(TOPDIR)/musl/$2
$(TOPDIR)/musl/$2: $(TOPDIR)/musl/$1
	$(LN) -sf $(TOPDIR)/musl/$1 $(TOPDIR)/musl/$2
endef

$(eval $(call musl_link,arch/${ARCH}/bits,arch/halvm/bits))
$(eval $(call musl_link,crt/${ARCH},crt/halvm))
$(eval $(call musl_link,src/fenv/${ARCH},src/fenv/halvm))
$(eval $(call musl_link,src/process/${ARCH},src/process/halvm))
$(eval $(call musl_link,src/setjmp/${ARCH},src/setjmp/halvm))
$(eval $(call musl_link,src/signal/${ARCH},src/signal/halvm))
$(eval $(call musl_link,src/string/${ARCH},src/string/halvm))
$(eval $(call musl_link,arch/${ARCH}/reloc.h,arch/halvm/reloc.h))
$(eval $(call musl_link,arch/${ARCH}/atomic_arch.h,arch/halvm/atomic_arch.h))
$(eval $(call musl_link,arch/generic/bits/resource.h,arch/halvm/bits/resource.h))
$(eval $(call musl_link,arch/generic/bits/termios.h,arch/halvm/bits/termios.h))

$(TOPDIR)/musl/config.mak: $(MUSL_DEPENDS)
	(cd musl && ./configure --disable-shared --target=halvm CC=$(CC))
	$(SED) -i -e 's!^CROSS_COMPILE.*$$!!g' $@

$(TOPDIR)/musl/obj/include/bits/alltypes.h: $(TOPDIR)/musl/config.mak
	$(MAKE) -C musl

clean::
	$(RM) $(MUSL_DEPENDS)
	$(MAKE) -C musl clean
	rm -f musl/config.mak

foo:
	echo $(MUSL_DEPENDS)

all:: $(TOPDIR)/musl/obj/include/bits/alltypes.h

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

$(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/base/ghc.mk \
                                  $(TOPDIR)/obj/include/bits/alltypes.h
	(cd halvm-ghc && RELEASE=YES ./configure $(HALVM_GHC_CONFIGURE_FLAGS))
	cp halvm-ghc/libraries/base/config.sub halvm-ghc/libraries/unix/config.sub

$(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage2: $(TOPDIR)/halvm-ghc/mk/config.mk
	$(MAKE) -C halvm-ghc ghclibdir=$(halvmlibdir)

# all:: $(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage2

clean::
	$(RM) -f $(TOPDIR)/halvm-ghc/mk/config.mk
	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/base.ghc.mk
	$(MAKE) -C halvm-ghc clean

