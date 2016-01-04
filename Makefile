# BANNERSTART
# - Copyright 2006-2015, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

.PHONY: all clean mrproper drproper install

all::
mrproper:: clean
drproper:: mrproper

include autoconf.mk
include platform-ghc.mk
include rumpkernel.mk

# Link our custom build.mk - controls the GHC build, forces Stage1Only etc
$(TOPDIR)/halvm-ghc/mk/build.mk: $(TOPDIR)/src/misc/build.mk
	$(LN) -sf $(TOPDIR)/src/misc/build.mk $@

clean::
	$(RM) -f $(TOPDIR)/halvm-ghc/mk/build.mk

# Run boot, for whatever it does.
$(TOPDIR)/halvm-ghc/libraries/base/ghc.mk: $(TOPDIR)/halvm-ghc/mk/build.mk
	(cd halvm-ghc && ./boot)

clean::
	$(RM) -rf $(TOPDIR)/halvm-ghc/libraries/*/*.mk

HALVM_GHC_CONFIGURE_FLAGS  = --target=$(TARGET_ARCH)
HALVM_GHC_CONFIGURE_FLAGS += --with-gcc=$(CC)
HALVM_GHC_CONFIGURE_FLAGS += --with-ld=$(LD)
HALVM_GHC_CONFIGURE_FLAGS += --with-nm=$(NM)
HALVM_GHC_CONFIGURE_FLAGS += --with-ar=$(AR)
HALVM_GHC_CONFIGURE_FLAGS += --with-objdump=$(OBJDUMP)
HALVM_GHC_CONFIGURE_FLAGS += --with-ranlib=$(RANLIB)
HALVM_GHC_CONFIGURE_FLAGS += --with-readelf=$(READELF)
HALVM_GHC_CONFIGURE_FLAGS += --with-ghc=$(PLATFORM_GHC)
HALVM_GHC_CONFIGURE_FLAGS += --prefix=$(prefix)

$(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/base/ghc.mk   \
                                  $(PLATFORM_GHC) $(PLATCABAL) $(PLATALEX)    \
                                  $(PLATHAPPY) $(PLATHADDOCK) $(PLATHSCOLOUR) \
                                  $(RUMPKERNEL_TARGET)
	(cd halvm-ghc && \
	  PATH=${PATH}:$(PLATFORM_GHC_DIR)/bin \
	  CPPFLAGS=-I$(RUMP_INCLUDE_DIR) \
	  ./configure $(HALVM_GHC_CONFIGURE_FLAGS))

clean::
	$(RM) -f $(TOPDIR)/halvm-ghc/mk/config.mk

$(TOPDIR)/src/misc/netbsd_compat.o: $(TOPDIR)/src/misc/netbsd_compat.c
	$(CC) -c -o $@ $<

HSC2HS_OPTS := -lflag=$(TOPDIR)/src/misc/netbsd_compat.o

clean::
	$(RM) -f $(TOPDIR)/src/misc/netbsd_compat.o

$(TOPDIR)/halvm-ghc/libraries/base/libc-include: \
       $(TOPDIR)/halvm-ghc/mk/config.mk $(RUMPKERNEL_TARGET)
	if [ ! -h $@ ]; then $(LN) -sf $(RUMP_INCLUDE_DIR) $@ ; fi

$(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1: $(TOPDIR)/halvm-ghc/mk/config.mk                \
                                            $(TOPDIR)/src/misc/netbsd_compat.o              \
                                            $(TOPDIR)/halvm-ghc/libraries/base/libc-include \
                                            $(RUMPKERNEL_TARGET)
	$(MAKE) -C halvm-ghc ghclibdir=$(halvmlibdir)

all:: $(TOPDIR)/halvm-ghc/inplace/bin/ghc-stage1

clean::
	$(MAKE) -C halvm-ghc clean

# 
# 
# # Link Xen headers into the HaLVM runtime include dir
# $(TOPDIR)/halvm-ghc/rts/xen/include/xen:
# 	$(LN) -sf $(XEN_INCLUDE_DIR)/xen $(TOPDIR)/halvm-ghc/rts/xen/include/xen
# 
# # Link HALVMCore into GHC's library path, where it will be found and built
# # by the GHC build system.
# $(TOPDIR)/halvm-ghc/libraries/HALVMCore: \
#        $(TOPDIR)/halvm-ghc/libraries/array/array.cabal
# 	if [ ! -h $@ ]; then \
# 	  $(LN) -sf $(TOPDIR)/src/HALVMCore $@ ; \
# 	fi
# 
# # Link XenDevice into GHC's library path, where it will be found and built
# # by the GHC build system.
# $(TOPDIR)/halvm-ghc/libraries/XenDevice: \
#        $(TOPDIR)/halvm-ghc/libraries/array/array.cabal
# 	if [ ! -h $@ ]; then \
# 	  $(LN) -sf $(TOPDIR)/src/XenDevice $@; \
# 	fi
# 
# # Replace libc headers with minlibc
# $(TOPDIR)/halvm-ghc/libraries/base/libc-include: \
#        $(TOPDIR)/halvm-ghc/libraries/base/GHC/Event/NoIO.hs
# 	if [ ! -h $@ ]; then \
# 	  $(LN) -sf $(TOPDIR)/halvm-ghc/rts/minlibc/include $@ ; \
# 	fi
# 
# GHC_PREPPED = $(TOPDIR)/halvm-ghc/libraries/base/GHC/Event/NoIO.hs \
#               $(TOPDIR)/halvm-ghc/rts/xen/include/xen              \
#               $(TOPDIR)/halvm-ghc/libraries/base/ghc.mk            \
#               $(TOPDIR)/halvm-ghc/libraries/base/libc-include      \
#               $(TOPDIR)/halvm-ghc/mk/build.mk                      \
#               $(TOPDIR)/halvm-ghc/libraries/HALVMCore              \
#               $(TOPDIR)/halvm-ghc/libraries/XenDevice
# mrproper::
# 	$(RM) -f $(TOPDIR)/halvm-ghc/rts/xen/include/xen
# 	$(RM) -f $(TOPDIR)/halvm-ghc/mk/build.mk
# 	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/HALVMCore
# 	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/XenDevice
# 	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/base/libc-include
# 
# ###############################################################################
# # GMP
# ################################################################################
# 
# ifeq ($(INTEGER_LIBRARY),integer-gmp)
# 
# $(TOPDIR)/src/gmp: | $(GHC_PREPPED)
# 	$(TAR) jxf $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/tarball/*.bz2
# 	$(MV) gmp-* $(TOPDIR)/src/gmp
# 
# $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp.h: $(TOPDIR)/src/gmp/.libs/libgmp.a
# 	$(LN) -sf $(TOPDIR)/src/gmp/gmp.h $@
# 
# $(TOPDIR)/halvm-ghc/libraries/integer-gmp/cbits/gmp.h: $(TOPDIR)/src/gmp/.libs/libgmp.a
# 	$(LN) -sf $(TOPDIR)/src/gmp/gmp.h $@
# 
# $(TOPDIR)/halvm-ghc/libraries/integer-gmp/.patched.config.sub: $(TOPDIR)/src/misc/hsgmp.patch
# 	(cd halvm-ghc/libraries/integer-gmp && $(PATCH) -p1 < $(TOPDIR)/src/misc/hsgmp.patch)
# 	$(TOUCH) $@
# 
# $(TOPDIR)/src/gmp/Makefile: | $(TOPDIR)/src/gmp
# 	(cd src/gmp && ABI="$(ABI)" CFLAGS="$(CFLAGS)" \
# 	    ./configure --disable-shared --enable-static)
# 
# $(TOPDIR)/src/gmp/.libs/libgmp.a: $(TOPDIR)/src/gmp/Makefile
# 	$(MAKE) -C src/gmp
# 
# all:: $(TOPDIR)/src/gmp/.libs/libgmp.a
# 
# install:: $(TOPDIR)/src/gmp/.libs/libgmp.a
# 	$(INSTALL) -D $(TOPDIR)/src/gmp/.libs/libgmp.a $(DESTDIR)$(halvmlibdir)/rts-1.0/libgmp.a
# 
# clean::
# 	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp.h
# 	$(RM) -f $(TOPDIR)/halvm-ghc/libraries/integer-gmp/cbits/gmp.h
# 	(cd $(TOPDIR)/halvm-ghc/libraries/integer-gmp && git reset --hard)
# 
# $(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp.h
# $(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/integer-gmp/cbits/gmp.h
# $(TOPDIR)/halvm-ghc/mk/config.mk: $(TOPDIR)/halvm-ghc/libraries/integer-gmp/.patched.config.sub
# endif
# 
# clean::
# 	$(RM) -rf src/gmp
# 
# ###############################################################################
# # LibM
# ################################################################################
# 
# $(TOPDIR)/src/openlibm/libopenlibm.a: $(LIBM_O_FILES)
# 	$(MAKE) -C $(TOPDIR)/src/openlibm all
# 
# all:: $(TOPDIR)/src/openlibm/libopenlibm.a
# 
# clean::
# 	$(MAKE) -C $(TOPDIR)/src/openlibm clean
# 
# install:: $(TOPDIR)/src/openlibm/libopenlibm.a
# 	$(INSTALL) -D $(TOPDIR)/src/openlibm/libopenlibm.a \
# 	              $(DESTDIR)$(halvmlibdir)/rts-1.0/libopenlibm.a
# 
# ###############################################################################
# # LibIVC
# ###############################################################################
# 
# LIBIVC_C_FILES := $(shell find $(TOPDIR)/src/libIVC -name '*.c')
# LIBIVC_HEADERS := $(shell find $(TOPDIR)/src/libIVC -name '*.h')
# LIBIVC_O_FILES := $(LIBIVC_C_FILES:.c=.o)
# 
# $(LIBIVC_C_FILES:.c=.o): %.o: %.c $(LIBIVC_HEADERS)
# 	$(CC) -o $@ $(CFLAGS) -I$(TOPDIR)/src/libIVC -c $<
# 
# $(TOPDIR)/src/libIVC/libIVC.a: $(LIBIVC_O_FILES)
# 	$(AR) rcs $@ $(LIBIVC_O_FILES)
# 
# all:: $(TOPDIR)/src/libIVC/libIVC.a
# 
# clean::
# 	$(RM) -f $(LIBIVC_O_FILES) $(TOPDIR)/src/libIVC/libIVC.a
# 
# install:: $(TOPDIR)/src/libIVC/libIVC.a
# 	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.a $(DESTDIR)$(libdir)/libIVC.a
# 	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.h $(DESTDIR)$(incdir)/libIVC.h
# 
# ###############################################################################
# # convert-profile
# ###############################################################################
# 
# $(TOPDIR)/src/profiling/convert-profile: $(TOPDIR)/src/profiling/convert-profile.c
# 	$(CC) -O2 -o $@ $<
# 
# all:: $(TOPDIR)/src/profiling/convert-profile
# 
# clean::
# 	$(RM) -f $(TOPDIR)/src/profiling/convert-profile
# 
# install:: $(TOPDIR)/src/profiling/convert-profile
# 	$(INSTALL) -D $(TOPDIR)/src/profiling/convert-profile $(DESTDIR)$(bindir)/convert-profile
# 
# ###############################################################################
# # MK_REND_DIR
# ###############################################################################
# 
# MKREND_C_FILES := $(shell find $(TOPDIR)/src/mkrenddir -name '*.c')
# MKREND_HEADERS := $(shell find $(TOPDIR)/src/mkrenddir -name '*.h')
# MKREND_O_FILES := $(MKREND_C_FILES:.c=.o)
# 
# $(MKREND_C_FILES:.c=.o): %.o: %.c $(MKREND_HEADERS)
# 	$(CC) -o $@ $(CFLAGS) -c $<
# 
# $(TOPDIR)/src/mkrenddir/mkrenddir: $(MKREND_O_FILES)
# 	$(CC) -o $@ $^ $(LDFLAGS) -lxenstore
# 
# all:: $(TOPDIR)/src/mkrenddir/mkrenddir
# 
# clean::
# 	$(RM) -f $(MKREND_O_FILES) $(TOPDIR)/src/mkrenddir/mkrenddir
# 
# install:: $(TOPDIR)/src/mkrenddir/mkrenddir
# 	$(INSTALL) -D $(TOPDIR)/src/mkrenddir/mkrenddir $(DESTDIR)$(bindir)/mkrenddir
# 
# ###############################################################################
# # Boot loader
# ###############################################################################
# 
# $(TOPDIR)/src/bootloader/start.o: $(TOPDIR)/src/bootloader/start.$(ARCH).S    \
#                                   $(wildcard $(TOPDIR)/src/bootloader/*.h)
# 	$(CC) -o $@ $(ASFLAGS) -I$(XEN_INCLUDE_DIR) -I$(TOPDIR)/src/bootloader -c $<
# 
# all:: $(TOPDIR)/src/bootloader/start.o
# 
# clean::
# 	rm -f $(TOPDIR)/src/bootloader/start.o
# 
# install::$(TOPDIR)/src/bootloader/start.o
# 	$(INSTALL) -D $(TOPDIR)/src/bootloader/start.o $(DESTDIR)$(halvmlibdir)/rts-1.0/start.o
# 
# ###############################################################################
# # The HaLVM!
# ###############################################################################
# 
# install::
# 	$(MAKE) -C halvm-ghc install ghclibdir=$(halvmlibdir) DESTDIR=$(DESTDIR)
# 	$(MKDIR) -p $(DESTDIR)$(halvmlibdir)/include/minlibc
# 	$(CP) -rf halvm-ghc/rts/minlibc/include/* $(DESTDIR)$(halvmlibdir)/include/minlibc
# 	$(SED) -i -e "s/^extra-ghci-libraries:/extra-ghci-libraries: minlibc/" \
# 	  $(DESTDIR)$(halvmlibdir)/package.conf.d/base*.conf
# 
# MINLIBC_SRCS      = $(wildcard $(TOPDIR)/halvm-ghc/rts/minlibc/*.c)
# GHCI_MINLIBC_SRCS = $(filter-out %termios.c,$(MINLIBC_SRCS))
# GHCI_MINLIBC_OBJS = $(patsubst $(TOPDIR)/halvm-ghc/rts/minlibc/%.c,           \
#                                $(TOPDIR)/halvm-ghc/rts/dist/build/minlibc/%.o,\
#                                $(GHCI_MINLIBC_SRCS))
# GHCI_OBJS         = $(GHCI_MINLIBC_OBJS) $(TOPDIR)/src/misc/ghci_runtime.o
# BASE_CABAL_FILE   = $(TOPDIR)/halvm-ghc/libraries/base/base.cabal
# BASE_VERSION      = \
#   $(shell grep "^version:" $(BASE_CABAL_FILE) | sed 's/^version:[ ]*//')
# 
# $(TOPDIR)/src/misc/ghci_runtime.o: $(TOPDIR)/src/misc/ghci_runtime.c
# 	$(CC) -c -o $@ $<
# 
# $(TOPDIR)/halvm-ghc/libminlibc.a:                                            \
#          $(TOPDIR)/halvm-ghc/rts/dist/build/libHSrts.a                       \
# 		 $(TOPDIR)/src/misc/ghci_runtime.o
# 	$(AR) cr $@ $(GHCI_OBJS)
# 
# all:: $(TOPDIR)/halvm-ghc/libminlibc.a
# 
# install::
# 	$(INSTALL) -D $(TOPDIR)/halvm-ghc/libminlibc.a \
# 	              $(DESTDIR)$(halvmlibdir)/base-$(BASE_VERSION)/libminlibc.a
# 
# install:: $(TOPDIR)/src/scripts/halvm-cabal
# 	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-cabal $(DESTDIR)$(bindir)/halvm-cabal
# 
# install:: $(TOPDIR)/src/scripts/halvm-config
# 	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-cabal $(DESTDIR)$(bindir)/halvm-config
# 
# install:: $(TOPDIR)/src/scripts/halvm-ghc
# 	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-ghc $(DESTDIR)$(bindir)/halvm-ghc
# 
# install:: $(TOPDIR)/src/scripts/halvm-ghc-pkg
# 	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-ghc-pkg $(DESTDIR)$(bindir)/halvm-ghc-pkg
# 
# install:: $(TOPDIR)/src/scripts/ldkernel
# 	$(INSTALL) -D $(TOPDIR)/src/scripts/ldkernel $(DESTDIR)$(halvmlibdir)/ldkernel
# 
# install:: $(TOPDIR)/src/misc/kernel-$(ARCH).lds
# 	$(INSTALL) -D $(TOPDIR)/src/misc/kernel-$(ARCH).lds $(DESTDIR)$(halvmlibdir)/kernel.lds
# 
# install:: ${PLATGHC}
# 	$(INSTALL) -D $(shell $(PLATGHC) --print-libdir)/bin/hsc2hs $(DESTDIR)${halvmlibdir}/bin/hsc2hs.bin
# 
# # Need to be sure we grab datadirs for alex and happy, /usr/share w.r.t. their prefix
# install:: $(PLATALEX) $(PLATCABAL) $(PLATHAPPY) $(PLATHADDOCK) $(PLATHSCOLOUR)
# 	mkdir -p $(DESTDIR)${halvmlibdir}
# 	cp -rf $(TOPDIR)/platform_ghc/${prefix}/* $(DESTDIR)${prefix}/
# 
# # hsc2hs requires a bunch of libraries to be installed. This is a hack (FIXME)
# # to copy over the platform_ghc ones to our destination directory and hope
# # nothing gets broken. Long term, finding some way to build a statically-linked
# # hsc2hs would be better.
# install::
# 	$(FIND) $(TOPDIR)/platform_ghc -name "*so" -name '*-ghc*' \
# 	    -exec cp '{}' $(DESTDIR)$(halvmlibdir)/lib/ \;
# 	$(INSTALL) -D $(TOPDIR)/src/scripts/hsc2hs $(DESTDIR)${halvmlibdir}/bin/hsc2hs
# 
