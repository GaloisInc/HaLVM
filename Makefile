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
all:

.PHONY: clean
clean::

.PHONY: install
install::

###############################################################################
# GHC Sub-library download / setup ############################################
###############################################################################

$(TOPDIR)/halvm-ghc/.sync: $(TOPDIR)/halvm-ghc/sync-all \
                           $(TOPDIR)/halvm-ghc/packages
	(cd halvm-ghc && ./sync-all --no-dph -r http://darcs.haskell.org/ get \
                && touch $@)

$(TOPDIR)/halvm-ghc/.halvm-base: $(TOPDIR)/halvm-ghc/.sync
	(cd halvm-ghc                                                               \
  && $(RM) -rf libraries/base                                                 \
  && $(GIT) clone $(GIT_LIB_URL)/halvm-base.git --branch halvm libraries/base \
  && $(TOUCH) $@)

EVERYTHING_DOWNLOADED := $(TOPDIR)/halvm-ghc/.sync \
                         $(TOPDIR)/halvm-ghc/.halvm-base

###############################################################################
# GMP #########################################################################
###############################################################################
ifeq ($(INTEGER_LIBRARY),integer-gmp)

$(TOPDIR)/src/gmp: | $(EVERYTHING_DOWNLOADED)
	$(TAR) jxf $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/tarball/*.bz2
	$(MV) gmp-5.0.3 $(TOPDIR)/src/gmp

$(TOPDIR)/src/gmp/Makefile: | $(TOPDIR)/src/gmp
	(cd src/gmp && ABI="$(ABI)" CFLAGS="$(CFLAGS)" \
   ./configure --disable-shared --enable-static )

$(TOPDIR)/src/gmp/.libs/libgmp.a: $(TOPDIR)/src/gmp/Makefile
	$(MAKE) -C src/gmp

all: $(TOPDIR)/src/gmp/.libs/libgmp.a

clean::
	$(RM) -rf src/gmp

install:: $(TOPDIR)/src/gmp/.libs/libgmp.a
	$(INSTALL) -D $(TOPDIR)/src/gmp/.libs/libgmp.a $(halvmlibdir)/rts-1.0/libgmp.a

endif
###############################################################################
# LIBM ########################################################################
###############################################################################

$(TOPDIR)/src/openlibm/libopenlibm.a: $(LIBM_O_FILES)
	$(MAKE) -C $(TOPDIR)/src/openlibm all

all: $(TOPDIR)/src/openlibm/libopenlibm.a

clean::
	$(MAKE) -C $(TOPDIR)/src/openlibm clean

install:: $(TOPDIR)/src/openlibm/libopenlibm.a
	$(INSTALL) -D $(TOPDIR)/src/openlibm/libopenlibm.a \
	              $(halvmlibdir)/rts-1.0/libopenlibm.a

###############################################################################
# LIBIVC ######################################################################
###############################################################################

LIBIVC_C_FILES := $(shell find $(TOPDIR)/src/libIVC -name '*.c')
LIBIVC_HEADERS := $(shell find $(TOPDIR)/src/libIVC -name '*.h')
LIBIVC_O_FILES := $(LIBIVC_C_FILES:.c=.o)

$(LIBIVC_C_FILES:.c=.o): %.o: %.c $(LIBIVC_HEADERS)
	$(CC) -o $@ $(CFLAGS) -I$(TOPDIR)/src/libIVC -c $<

$(TOPDIR)/src/libIVC/libIVC.a: $(LIBIVC_O_FILES)
	$(AR) rcs $@ $(LIBIVC_O_FILES)

all: $(TOPDIR)/src/libIVC/libIVC.a

clean::
	$(RM) -f $(LIBIVC_O_FILES) $(TOPDIR)/src/libIVC/libIVC.a

install:: $(TOPDIR)/src/libIVC/libIVC.a
	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.a $(libdir)/libIVC.a
	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.h $(incdir)/libIVC.h

###############################################################################
# MK_REND_DIR #################################################################
###############################################################################

MKREND_C_FILES := $(shell find $(TOPDIR)/src/mkrenddir -name '*.c')
MKREND_HEADERS := $(shell find $(TOPDIR)/src/mkrenddir -name '*.h')
MKREND_O_FILES := $(MKREND_C_FILES:.c=.o)

$(MKREND_C_FILES:.c=.o): %.o: %.c $(MKREND_HEADERS)
	$(CC) -o $@ $(CFLAGS) -c $<

$(TOPDIR)/src/mkrenddir/mkrenddir: $(MKREND_O_FILES)
	$(CC) -o $@ $^ $(LDFLAGS) -lxenstore

all: $(TOPDIR)/src/mkrenddir/mkrenddir

clean::
	$(RM) -f $(MKREND_O_FILES) $(TOPDIR)/src/mkrenddir/mkrenddir

install:: $(TOPDIR)/src/mkrenddir/mkrenddir
	$(INSTALL) -D $(TOPDIR)/src/mkrenddir/mkrenddir $(bindir)/mkrenddir

###############################################################################
# BOOTLOADER ##################################################################
###############################################################################

$(TOPDIR)/src/bootloader/start.o: $(TOPDIR)/src/bootloader/start.$(ARCH).S    \
                                  $(wildcard $(TOPDIR)/src/bootloader/*.h)
	$(CC) -o $@ $(ASFLAGS) -I$(XEN_INCLUDE_DIR) -I$(TOPDIR)/src/bootloader -c $<

all: $(TOPDIR)/src/bootloader/start.o

clean::
	rm -f $(TOPDIR)/src/bootloader/start.o

install::$(TOPDIR)/src/bootloader/start.o
	$(INSTALL) -D $(TOPDIR)/src/bootloader/start.o $(halvmlibdir)/rts-1.0/start.o

###############################################################################
# GHC BUILD PREP ##############################################################
###############################################################################

ifeq (${FORCE_PLATFORM_GHC},yes)
$(GHC_FILE):
	$(CURL) -O $(GHC_LINK)

$(GHC): $(GHC_FILE)
	$(RM) -rf ghc-7.8.2
	$(TAR) jxf $(GHC_FILE)
	(cd ghc-7.8.2 && ./configure --prefix=$(TOPDIR)/platform_ghc)
	$(MAKE) -C ghc-7.8.2 install
	PATH=$(dir $(GHC)):${PATH} $(CABAL) install --global --prefix=$(TOPDIR)/platform_ghc terminfo
	$(RM) -rf ghc-7.8.2

clean::
	$(RM) -rf ghc-7.8.2
endif

$(TOPDIR)/halvm-ghc/mk/build.mk: $(EVERYTHING_DOWNLOADED) \
                                 $(TOPDIR)/src/misc/build.mk
	$(CP) $(TOPDIR)/src/misc/build.mk $@

$(TOPDIR)/halvm-ghc/.linked-xen: $(EVERYTHING_DOWNLOADED)
	$(LN) -sf $(XEN_INCLUDE_DIR)/xen $(TOPDIR)/halvm-ghc/rts/xen/include/xen
	$(TOUCH) $@

$(TOPDIR)/halvm-ghc/.linked-rts: $(EVERYTHING_DOWNLOADED)
	$(LN) -sf $(TOPDIR)/halvm-ghc/rts/minlibc/include \
            $(TOPDIR)/halvm-ghc/libraries/base/libc-include
	$(TOUCH) $@

$(TOPDIR)/halvm-ghc/configure: $(EVERYTHING_DOWNLOADED)                       \
                               $(TOPDIR)/halvm-ghc/configure.ac               \
                               $(TOPDIR)/halvm-ghc/boot                       \
							   $(GHC)
	(cd halvm-ghc && ./boot)

$(TOPDIR)/halvm-ghc/libraries/HALVMCore: $(EVERYTHING_DOWNLOADED)
	$(LN) -sf $(TOPDIR)/src/HALVMCore $(TOPDIR)/halvm-ghc/libraries/HALVMCore

$(TOPDIR)/halvm-ghc/libraries/XenDevice: $(EVERYTHING_DOWNLOADED)
	$(LN) -sf $(TOPDIR)/src/XenDevice $(TOPDIR)/halvm-ghc/libraries/XenDevice

EVERYTHING_PREPPED := $(TOPDIR)/halvm-ghc/mk/build.mk                         \
                      $(TOPDIR)/halvm-ghc/.linked-xen                         \
                      $(TOPDIR)/halvm-ghc/.linked-rts                         \
                      $(TOPDIR)/halvm-ghc/libraries/HALVMCore                 \
                      $(TOPDIR)/halvm-ghc/libraries/XenDevice                 \
                      $(TOPDIR)/halvm-ghc/configure

ifeq ($(INTEGER_LIBRARY),integer-gmp)
$(TOPDIR)/halvm-ghc/.linked-gmp: \
                                         $(TOPDIR)/src/gmp/.libs/libgmp.a
	$(LN) -sf $(TOPDIR)/src/gmp/gmp.h \
            $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp.h
	$(LN) -sf $(TOPDIR)/src/gmp/gmp.h \
            $(TOPDIR)/halvm-ghc/libraries/integer-gmp/cbits/gmp.h
	$(TOUCH) $@

$(TOPDIR)/halvm-ghc/.fixed-gmp: $(EVERYTHING_DOWNLOADED)
	(cd halvm-ghc/libraries/integer-gmp && $(PATCH) -p1 < $(TOPDIR)/src/misc/hsgmp.patch)
	$(TOUCH) $@

EVERYTHING_PREPPED += $(TOPDIR)/halvm-ghc/.linked-gmp \
                      $(TOPDIR)/halvm-ghc/.fixed-gmp
endif

clean::
	$(RM) -f $(EVERYTHING_PREPPED) $(TOPDIR)/halvm-ghc/rts/xen/include/xen \
           $(TOPDIR)/halvm-ghc/libraries/base/include/rts

###############################################################################
# GHC BUILD ###################################################################
###############################################################################

HALVM_GHC_CONFIGURE_FLAGS  = --target=$(TARGET_ARCH)
HALVM_GHC_CONFIGURE_FLAGS += --with-gcc=$(CC)
HALVM_GHC_CONFIGURE_FLAGS += --with-ld=$(LD)
HALVM_GHC_CONFIGURE_FLAGS += --with-nm=$(NM)
HALVM_GHC_CONFIGURE_FLAGS += --with-ar=$(AR)
HALVM_GHC_CONFIGURE_FLAGS += --with-objdump=$(OBJDUMP)
HALVM_GHC_CONFIGURE_FLAGS += --with-ranlib=$(RANLIB)
HALVM_GHC_CONFIGURE_FLAGS += --prefix=$(prefix)

COMPILER_SOURCES := $(shell find $(TOPDIR)/halvm-ghc/compiler     \
                            -name '*hs' ! -path '*dist*')
LIBRARY_SOURCES  := $(shell find $(TOPDIR)/halvm-ghc/libraries    \
                            -name '*hs' ! -path '*dist*')
RTS_SOURCES      := $(shell find $(TOPDIR)/halvm-ghc/rts          \
                            -name '*.c')

$(TOPDIR)/halvm-ghc/config.log: $(EVERYTHING_PREPPED)                          \
                                $(TOPDIR)/halvm-ghc/configure
	(cd halvm-ghc                                                              \
   && PATH=$(dir $(GHC)):${PATH} ./configure $(HALVM_GHC_CONFIGURE_FLAGS)      \
   && $(TOUCH) $@)

$(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1:                                    \
         $(EVERYTHING_PREPPED)                                                 \
         $(TOPDIR)/halvm-ghc/config.log                                        \
         $(COMPILER_SOURCES) $(LIBRARY_SOURCES)
	$(MAKE) -C halvm-ghc ghclibdir=$(halvmlibdir)

$(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts.a:                                 \
         $(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1                            \
         $(RTS_SOURCES)
	$(MAKE) -C halvm-ghc rts/dist/build/libHSrts.a ghclibdir=$(halvmlibdir)

$(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts_thr.a:                             \
         $(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1                            \
         $(RTS_SOURCES)
	$(MAKE) -C halvm-ghc rts/dist/build/libHSrts_thr.a ghclibdir=$(halvmlibdir)

all: $(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1                                \
     $(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts.a                             \
     $(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts_thr.a

clean::
	$(MAKE) -C halvm-ghc clean

install::
	$(MAKE) -C halvm-ghc install ghclibdir=$(halvmlibdir)
	$(MKDIR) -p $(halvmlibdir)/include/minlibc
	$(CP) -rf halvm-ghc/rts/minlibc/include/* $(halvmlibdir)/include/minlibc/
	$(SED) -i -e "s/^extra-ghci-libraries:/extra-ghci-libraries: minlibc/"   \
           $(halvmlibdir)/package.conf.d/base*.conf

###############################################################################
# GHC BUILD ###################################################################
###############################################################################

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

all: $(TOPDIR)/halvm-ghc/libminlibc.a

install::
	$(INSTALL) -D $(TOPDIR)/halvm-ghc/libminlibc.a \
	              $(halvmlibdir)/base-$(BASE_VERSION)/libminlibc.a

###############################################################################
# HaLVM SCRIPTS ###############################################################
###############################################################################

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


