# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

include autoconf.mk

.PHONY: all
all::

.PHONY: clean
clean::

.PHONY: mrproper
mrproper:: clean

.PHONY: install
install::

###############################################################################
# File Downloading
###############################################################################

$(GHC_FILE):
	$(CURL) -LO $(GHC_LINK)

$(CABAL_FILE):
	$(CURL) -LO $(CABAL_LINK)

mrproper::
	$(RM) -f $(GHC_FILE)
	$(RM) -f $(CABAL_FILE)

###############################################################################
# Platform GHC Preparation
###############################################################################

ifeq ($(GHC),no)
PLATGHC    = $(TOPDIR)/platform_ghc/bin/ghc

$(PLATGHC): $(GHC_FILE)
	$(eval gtmpdir = $(shell mktemp -d))
	$(TAR) jxf $(GHC_FILE) -C $(gtmpdir)
	(cd $(gtmpdir)/ghc* && ./configure --prefix=$(TOPDIR)/platform_ghc)
	$(MAKE) -C $(gtmpdir)/ghc*/ install
	$(RM) -rf $(gtmpdir)

mrproper::
	$(RM) -rf $(TOPDIR)/platform_ghc
	$(RM) $(HOME)/.ghc/$(ARCH)-linux-7.8.4
else
# Use the global GHC.
PLATGHC    = $(GHC)
endif

ifeq ($(CABAL),no)
PLATCABAL = $(TOPDIR)/platform_ghc/bin/cabal

$(PLATCABAL): $(CABAL_FILE) $(PLATGHC)
	$(eval ctmpdir = $(shell mktemp -d))
	echo $(ctmpdir)
	$(TAR) zxf $(CABAL_FILE) -C $(ctmpdir)
	$(RM) -rf ${HOME}/.ghc/${ARCH}-linux-7.8.4
	(cd $(ctmpdir)/cabal* && PREFIX=$(TOPDIR)/platform_ghc \
	  						 GHC=$(PLATGHC)                \
							 GHC_PKG=$(PLATGHC)-pkg ./bootstrap.sh --no-doc)
	$(RM) -rf $(ctmpdir)
	$(PLATCABAL) update

mrproper::
	$(RM) -rf $(TOPDIR)/platform_ghc
else
PLATCABAL = $(CABAL)
endif

ifeq ($(ALEX),no)
PLATALEX = $(TOPDIR)/platform_ghc/bin/alex

$(PLATALEX): $(PLATCABAL)
	env PATH=$(TOPDIR)/platform_ghc/bin:${PATH} \
           $(PLATCABAL) install --prefix=$(TOPDIR)/platform_ghc alex
else
PLATALEX = $(ALEX)
endif

ifeq ($(HAPPY),no)
PLATHAPPY = $(TOPDIR)/platform_ghc/bin/happy

$(PLATHAPPY): $(PLATCABAL)
	env PATH=$(TOPDIR)/platform_ghc/bin:${PATH} \
	   $(PLATCABAL) install --prefix=$(TOPDIR)/platform_ghc happy
else
PLATHAPPY = $(HAPPY)
endif

ifeq ($(HADDOCK),no)
PLATHADDOCK = $(TOPDIR)/platform_ghc/bin/haddock

$(PLATHADDOCK): $(PLATCABAL)
	env PATH=$(TOPDIR)/platform_ghc/bin:${PATH} \
	   $(PLATCABAL) install --prefix=$(TOPDIR)/platform_ghc haddock
else
PLATHADDOCK = $(HADDOCK)
endif

ifeq ($(HSCOLOUR),no)
PLATHSCOLOUR = $(TOPDIR)/platform_ghc/bin/hscolour

$(PLATHSCOLOUR): $(PLATCABAL)
	env PATH=$(TOPDIR)/platform_ghc/bin:${PATH} \
	   $(PLATCABAL) install --prefix=$(TOPDIR)/platform_ghc hscolour
else
PLATHSCOLOUR = $(HSCOLOUR)
endif

###############################################################################
# Prepping / supporting the GHC build
################################################################################

$(TOPDIR)/halvm-ghc/libraries/array/array.cabal:
	(cd halvm-ghc && ./sync-all --no-dph -r http://darcs.haskell.org get)
	(cd halvm-ghc && ./sync-all checkout -t origin/ghc-7.8)

$(TOPDIR)/halvm-ghc/libraries/base/GHC/Event/NoIO.hs: \
             $(TOPDIR)/halvm-ghc/libraries/array/array.cabal
	$(RM) -rf $(TOPDIR)/halvm-ghc/libraries/base
	$(GIT) clone $(GIT_LIB_URL)/halvm-base.git -b halvm halvm-ghc/libraries/base

$(TOPDIR)/halvm-ghc/libraries/base/ghc.mk: \
             $(TOPDIR)/halvm-ghc/libraries/base/GHC/Event/NoIO.hs \
			 $(TOPDIR)/halvm-ghc/mk/build.mk
	(cd halvm-ghc && ./boot)

$(TOPDIR)/halvm-ghc/rts/xen/include/xen:
	$(LN) -sf $(XEN_INCLUDE_DIR)/xen $(TOPDIR)/halvm-ghc/rts/xen/include/xen

$(TOPDIR)/halvm-ghc/mk/build.mk: $(TOPDIR)/src/misc/build.mk
	$(LN) -sf $(TOPDIR)/src/misc/build.mk $@

$(TOPDIR)/halvm-ghc/libraries/HALVMCore: \
       $(TOPDIR)/halvm-ghc/libraries/array/array.cabal
	if [ ! -h $@ ]; then \
	  $(LN) -sf $(TOPDIR)/src/HALVMCore $@ ; \
	fi

$(TOPDIR)/halvm-ghc/libraries/XenDevice: \
       $(TOPDIR)/halvm-ghc/libraries/array/array.cabal
	if [ ! -h $@ ]; then \
	  $(LN) -sf $(TOPDIR)/src/XenDevice $@; \
	fi

$(TOPDIR)/halvm-ghc/libraries/base/libc-include: \
       $(TOPDIR)/halvm-ghc/libraries/base/GHC/Event/NoIO.hs
	if [ ! -h $@ ]; then \
	  $(LN) -sf $(TOPDIR)/halvm-ghc/rts/minlibc/include $@ ; \
	fi

GHC_PREPPED = $(TOPDIR)/halvm-ghc/libraries/base/GHC/Event/NoIO.hs \
              $(TOPDIR)/halvm-ghc/rts/xen/include/xen              \
              $(TOPDIR)/halvm-ghc/libraries/base/ghc.mk            \
              $(TOPDIR)/halvm-ghc/libraries/base/libc-include      \
              $(TOPDIR)/halvm-ghc/mk/build.mk                      \
              $(TOPDIR)/halvm-ghc/libraries/HALVMCore              \
              $(TOPDIR)/halvm-ghc/libraries/XenDevice
mrproper::
	$(RM) -f $(TOPDIR)/halvm-ghc/rts/xen/include/xen
	$(RM) -f $(TOPDIR)/halvm-ghc/mk/build.mk
	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/HALVMCore
	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/XenDevice
	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/base/libc-include

###############################################################################
# GMP
################################################################################

ifeq ($(INTEGER_LIBRARY),integer-gmp)

$(TOPDIR)/src/gmp: | $(GHC_PREPPED)
	$(TAR) jxf $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/tarball/*.bz2
	$(MV) gmp-* $(TOPDIR)/src/gmp

$(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp.h: $(TOPDIR)/src/gmp/.libs/libgmp.a
	$(LN) -sf $(TOPDIR)/src/gmp/gmp.h $@

$(TOPDIR)/halvm-ghc/libraries/integer-gmp/cbits/gmp.h: $(TOPDIR)/src/gmp/.libs/libgmp.a
	$(LN) -sf $(TOPDIR)/src/gmp/gmp.h $@

$(TOPDIR)/halvm-ghc/libraries/integer-gmp/.patched.config.sub: $(TOPDIR)/src/misc/hsgmp.patch
	(cd halvm-ghc/libraries/integer-gmp && $(PATCH) -p1 < $(TOPDIR)/src/misc/hsgmp.patch)
	$(TOUCH) $@

$(TOPDIR)/src/gmp/Makefile: | $(TOPDIR)/src/gmp
	(cd src/gmp && ABI="$(ABI)" CFLAGS="$(CFLAGS)" \
	    ./configure --disable-shared --enable-static)

$(TOPDIR)/src/gmp/.libs/libgmp.a: $(TOPDIR)/src/gmp/Makefile
	$(MAKE) -C src/gmp

all:: $(TOPDIR)/src/gmp/.libs/libgmp.a

install:: $(TOPDIR)/src/gmp/.libs/libgmp.a
	$(INSTALL) -D $(TOPDIR)/src/gmp/.libs/libgmp.a $(halvmlibdir)/rts-1.0/libgmp.a

clean::
	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp.h
	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/integer-gmp/cbits/gmp.h
	(cd $(TOPDIR)/halvm-ghc/libraries/integer-gmp && git reset --hard)

$(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp.h
$(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/integer-gmp/cbits/gmp.h
$(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/integer-gmp/.patched.config.sub
endif

clean::
	$(RM) -rf src/gmp

###############################################################################
# LibM
################################################################################

$(TOPDIR)/src/openlibm/libopenlibm.a: $(LIBM_O_FILES)
	$(MAKE) -C $(TOPDIR)/src/openlibm all

all:: $(TOPDIR)/src/openlibm/libopenlibm.a

clean::
	$(MAKE) -C $(TOPDIR)/src/openlibm clean

install:: $(TOPDIR)/src/openlibm/libopenlibm.a
	$(INSTALL) -D $(TOPDIR)/src/openlibm/libopenlibm.a \
	              $(halvmlibdir)/rts-1.0/libopenlibm.a

###############################################################################
# LibIVC
###############################################################################

LIBIVC_C_FILES := $(shell find $(TOPDIR)/src/libIVC -name '*.c')
LIBIVC_HEADERS := $(shell find $(TOPDIR)/src/libIVC -name '*.h')
LIBIVC_O_FILES := $(LIBIVC_C_FILES:.c=.o)

$(LIBIVC_C_FILES:.c=.o): %.o: %.c $(LIBIVC_HEADERS)
	$(CC) -o $@ $(CFLAGS) -I$(TOPDIR)/src/libIVC -c $<

$(TOPDIR)/src/libIVC/libIVC.a: $(LIBIVC_O_FILES)
	$(AR) rcs $@ $(LIBIVC_O_FILES)

all:: $(TOPDIR)/src/libIVC/libIVC.a

clean::
	$(RM) -f $(LIBIVC_O_FILES) $(TOPDIR)/src/libIVC/libIVC.a

install:: $(TOPDIR)/src/libIVC/libIVC.a
	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.a $(libdir)/libIVC.a
	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.h $(incdir)/libIVC.h

###############################################################################
# convert-profile
###############################################################################

$(TOPDIR)/src/profiling/convert-profile: $(TOPDIR)/src/profiling/convert-profile.c
	$(CC) -O2 -o $@ $<

all:: $(TOPDIR)/src/profiling/convert-profile

clean::
	$(RM) -f $(TOPDIR)/src/profiling/convert-profile

install:: $(TOPDIR)/src/profiling/convert-profile
	$(INSTALL) -D $(TOPDIR)/src/profiling/convert-profile $(bindir)/convert-profile

###############################################################################
# MK_REND_DIR
###############################################################################

MKREND_C_FILES := $(shell find $(TOPDIR)/src/mkrenddir -name '*.c')
MKREND_HEADERS := $(shell find $(TOPDIR)/src/mkrenddir -name '*.h')
MKREND_O_FILES := $(MKREND_C_FILES:.c=.o)

$(MKREND_C_FILES:.c=.o): %.o: %.c $(MKREND_HEADERS)
	$(CC) -o $@ $(CFLAGS) -c $<

$(TOPDIR)/src/mkrenddir/mkrenddir: $(MKREND_O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS) -lxenstore

all:: $(TOPDIR)/src/mkrenddir/mkrenddir

clean::
	$(RM) -f $(MKREND_O_FILES) $(TOPDIR)/src/mkrenddir/mkrenddir

install:: $(TOPDIR)/src/mkrenddir/mkrenddir
	$(INSTALL) -D $(TOPDIR)/src/mkrenddir/mkrenddir $(bindir)/mkrenddir

###############################################################################
# Boot loader
###############################################################################

$(TOPDIR)/src/bootloader/start.o: $(TOPDIR)/src/bootloader/start.$(ARCH).S    \
                                  $(wildcard $(TOPDIR)/src/bootloader/*.h)
	$(CC) -o $@ $(ASFLAGS) -I$(XEN_INCLUDE_DIR) -I$(TOPDIR)/src/bootloader -c $<

all:: $(TOPDIR)/src/bootloader/start.o

clean::
	rm -f $(TOPDIR)/src/bootloader/start.o

install::$(TOPDIR)/src/bootloader/start.o
	$(INSTALL) -D $(TOPDIR)/src/bootloader/start.o $(halvmlibdir)/rts-1.0/start.o

###############################################################################
# The HaLVM!
###############################################################################

HALVM_GHC_CONFIGURE_FLAGS  = --target=$(TARGET_ARCH)
HALVM_GHC_CONFIGURE_FLAGS += --with-gcc=$(CC)
HALVM_GHC_CONFIGURE_FLAGS += --with-ld=$(LD)
HALVM_GHC_CONFIGURE_FLAGS += --with-nm=$(NM)
HALVM_GHC_CONFIGURE_FLAGS += --with-ar=$(AR)
HALVM_GHC_CONFIGURE_FLAGS += --with-objdump=$(OBJDUMP)
HALVM_GHC_CONFIGURE_FLAGS += --with-ranlib=$(RANLIB)
HALVM_GHC_CONFIGURE_FLAGS += --with-ghc=$(PLATGHC)
HALVM_GHC_CONFIGURE_FLAGS += --prefix=$(prefix)

$(TOPDIR)/halvm-ghc/mk/config.mk: $(GHC_PREPPED) $(PLATGHC) $(PLATALEX) \
                                  $(PLATHAPPY) $(PLATHADDOCK) $(PLATHAPPY)
	(cd halvm-ghc && \
	  env PATH=$(TOPDIR)/platform_ghc/bin:${PATH} \
	    ./configure $(HALVM_GHC_CONFIGURE_FLAGS))

$(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1: $(TOPDIR)/halvm-ghc/mk/config.mk
	$(MAKE) -C halvm-ghc ghclibdir=$(halvmlibdir)

$(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts.a: $(TOPDIR)/halvm-ghc/mk/config.mk
	$(MAKE) -C halvm-ghc rts/dist/build/libHSrts.a ghclibdir=$(halvmlibdir)

$(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts_thr.a: $(TOPDIR)/halvm-ghc/mk/config.mk
	$(MAKE) -C halvm-ghc rts/dist/build/libHSrts_thr.a ghclibdir=$(halvmlibdir)

$(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts_p.a: $(TOPDIR)/halvm-ghc/mk/config.mk
	$(MAKE) -C halvm-ghc rts/dist/build/libHSrts_p.a ghclibdir=$(halvmlibdir)

all:: $(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1

all:: $(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts.a

all:: $(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts_thr.a

all:: $(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts_p.a

clean::
	$(MAKE) -C halvm-ghc clean

install::
	$(MAKE) -C halvm-ghc install ghclibdir=$(halvmlibdir)
	$(MKDIR) -p $(halvmlibdir)/include/minlibc
	$(CP) -rf halvm-ghc/rts/minlibc/include/* $(halvmlibdir)/include/minlibc
	$(SED) -i -e "s/^extra-ghci-libraries:/extra-ghci-libraries: minlibc/" \
	  $(halvmlibdir)/package.conf.d/base*.conf

MINLIBC_SRCS      = $(wildcard $(TOPDIR)/halvm-ghc/rts/minlibc/*.c)
GHCI_MINLIBC_SRCS = $(filter-out %termios.c,$(MINLIBC_SRCS))
GHCI_MINLIBC_OBJS = $(patsubst $(TOPDIR)/halvm-ghc/rts/minlibc/%.c,           \
                               $(TOPDIR)/halvm-ghc/rts/dist/build/minlibc/%.o,\
                               $(GHCI_MINLIBC_SRCS))
GHCI_OBJS         = $(GHCI_MINLIBC_OBJS) $(TOPDIR)/src/misc/ghci_runtime.o
BASE_CABAL_FILE   = $(TOPDIR)/halvm-ghc/libraries/base/base.cabal
BASE_VERSION      = \
  $(shell grep "^version:" $(BASE_CABAL_FILE) | sed 's/^version:[ ]*//')

$(TOPDIR)/src/misc/ghci_runtime.o: $(TOPDIR)/src/misc/ghci_runtime.c
	$(CC) -c -o $@ $<

$(TOPDIR)/halvm-ghc/libminlibc.a:                                            \
         $(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts.a                       \
		 $(TOPDIR)/src/misc/ghci_runtime.o
	$(AR) cr $@ $(GHCI_OBJS)

all:: $(TOPDIR)/halvm-ghc/libminlibc.a

install::
	$(INSTALL) -D $(TOPDIR)/halvm-ghc/libminlibc.a \
	              $(halvmlibdir)/base-$(BASE_VERSION)/libminlibc.a

install:: $(TOPDIR)/src/scripts/halvm-cabal
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-cabal $(bindir)/halvm-cabal

install:: $(TOPDIR)/src/scripts/halvm-config
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-cabal $(bindir)/halvm-config

install:: $(TOPDIR)/src/scripts/halvm-ghc
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-ghc $(bindir)/halvm-ghc

install:: $(TOPDIR)/src/scripts/halvm-ghc-pkg
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-ghc-pkg $(bindir)/halvm-ghc-pkg
	$(bindir)/halvm-ghc-pkg recache

install:: $(TOPDIR)/src/scripts/ldkernel
	$(INSTALL) -D $(TOPDIR)/src/scripts/ldkernel $(halvmlibdir)/ldkernel

install:: $(TOPDIR)/src/misc/kernel-$(ARCH).lds
	$(INSTALL) -D $(TOPDIR)/src/misc/kernel-$(ARCH).lds $(halvmlibdir)/kernel.lds

install:: ${PLATCABAL}
	$(INSTALL) -D ${PLATCABAL} ${halvmlibdir}/bin/cabal

PLATHSC2HS = $(shell $(PLATGHC) --print-libdir)/bin/hsc2hs
install:: ${PLATHSC2HS}
	$(INSTALL) -D ${PLATHSC2HS} ${halvmlibdir}/bin/hsc2hs

install:: ${PLATALEX}
	$(INSTALL) -D ${PLATALEX} ${halvmlibdir}/bin/alex

install:: ${PLATHAPPY}
	$(INSTALL) -D ${PLATHAPPY} ${halvmlibdir}/bin/happy

install:: ${PLATHADDOCK}
	$(INSTALL) -D ${PLATHADDOCK} ${halvmlibdir}/bin/haddock

