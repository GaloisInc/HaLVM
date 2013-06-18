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
include rts/build.mk

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
	  && $(RM) -rf libraries/base \
	  && $(GIT) clone $(GIT_LIBRARIES_URL)/halvm-base.git --branch halvm libraries/base \
	  && $(RM) -f rts/xen/include/xen \
	  && $(LN) -sf $(XEN_INCLUDE_DIR)/xen rts/xen/include/xen \
	  && $(RM) -f libraries/integer-gmp/gmp/gmp.h \
	  && $(LN) -sf $(TOPDIR)/gmp/gmp.h libraries/integer-gmp/gmp/gmp.h \
	  && $(RM) -f libraries/base/include/rts \
	  && $(LN) -sf $(TOPDIR)/halvm-ghc/rts/xen/include libraries/base/include/rts)

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
HALVM_GHC_CONFIGURE_FLAGS += --prefix=$(halvm-dir)

ifeq "$(USE_GMP)" "yes"
HALVM_GHC_CONFIGURE_FLAGS += --with-gmp-libraries=$(GMP_LIB_PATH)
endif

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

# GMP ##########################################################################

GMP_TAR = halvm-ghc/libraries/integer-gmp/gmp/tarball/gmp-5.0.3-nodoc-patched.tar.bz2
GMP_ABI = 64
GMP_CFLAGS = -O2 -fno-stack-protector

gmp/configure: halvm-ghc/mk/config.mk $(GMP_TAR)
	$(RM) -rf gmp gmp-5.0.3
	$(TAR) jxf $(GMP_TAR)
	$(MV) gmp-5.0.3 gmp

gmp/Makefile: gmp/configure
	(cd gmp && ABI="$(GMP_ABI)" CFLAGS="$(CFLAGS)" \
		./configure --disable-shared --enable-static )

gmp/.libs/libgmp.a: gmp/Makefile
	$(MAKE) -C gmp

# libIVC #######################################################################

LIBIVC_SRC = $(wildcard libIVC/src/*.c)
LIBIVC_OBJ = $(patsubst %.c,%.o,$(LIBIVC_SRC))

libIVC/src/%.o: CFLAGS += -IlibIVC/include
libIVC/src/%.o: libIVC/src/ivc_private.h
libIVC/src/%.o: libIVC/include/libIVC.h
libIVC/src/%.o: libIVC/src/%.c
	$(CC) $(CFLAGS) -c -o $@ $^

libIVC/libIVC.a: $(LIBIVC_OBJ)
	$(AR) cqs $@ $^

# Installation #################################################################

quiet_cmd_install = INSTALL   $@
      cmd_install = install -D $< $@

quiet_cmd_ln      = LN        $@
      cmd_ln      = ln -sf $< $@

programs := $(bindir)/halvm-ghc $(bindir)/halvm-ghc-pkg  \
            $(bindir)/halvm-cabal $(bindir)/halvm-config \
            $(bindir)/make_halvm_dir.py

rts_incls := $(shell find halvm-ghc/rts/xen/include -name '*.h')
incl_targs := $(patsubst halvm-ghc/rts/xen/%,$(halvm-dir)/%,$(rts_incls))

$(programs): $(bindir)/%: $(TOPDIR)/static-bits/bin/%
	$(call cmd,install)

$(incl_targs): $(halvm-dir)/%: $(TOPDIR)/halvm-ghc/rts/xen/%
	$(call cmd,install)

.PHONY: install-ghc
install-ghc:
	$(call label,halvm-ghc install)$(MAKE) -C halvm-ghc install

# linker script
$(halvm-dir)/ldkernel: static-bits/lib/ldkernel
	$(call cmd,install)

$(halvm-dir)/kernel.lds: static-bits/lib/kernel-$(ARCH).lds
	$(call cmd,install)

$(halvm-dir)/libgmp.a: gmp/.libs/libgmp.a
	$(call cmd,install)

$(halvm-dir)/include/xen: $(XEN_INCLUDE_DIR)/xen
	$(call cmd,ln)

install: $(programs) $(incl_targs) $(halvm-dir)/ldkernel $(halvm-dir)/kernel.lds
install: $(halvm-dir)/include/xen
install: $(halvm-dir)/libgmp.a
install: $(halvm-dir)/lib/ghc-7.7/include/libIVC.h
install: $(halvm-dir)/lib/ghc-7.7/libIVC.a
install: | install-ghc

$(halvm-dir)/lib/ghc-7.7/libIVC.a: libIVC/libIVC.a
	cp $^ $@

$(halvm-dir)/lib/ghc-7.7/include/libIVC.h: libIVC/include/libIVC.h
	cp $^ $@

foo:
	echo $(halvm-dir)
