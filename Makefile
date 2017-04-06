# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

include autoconf.mk

RELEASE=1

.PHONY: all
all::

.PHONY: clean
clean::

.PHONY: mrproper
mrproper:: clean

.PHONY: install
install::

###############################################################################
# Prepping / supporting the GHC build
################################################################################

$(TOPDIR)/halvm-ghc/libraries/base/ghc.mk: $(TOPDIR)/halvm-ghc/mk/build.mk
	(cd halvm-ghc && ./boot)

# Link Xen headers into the HaLVM runtime include dir
$(TOPDIR)/halvm-ghc/rts/xen/include/xen:
	$(LN) -sf $(XEN_INCLUDE_DIR)/xen $(TOPDIR)/halvm-ghc/rts/xen/include/xen

# Link our custom build.mk - controls the GHC build, forces Stage1Only etc
$(TOPDIR)/halvm-ghc/mk/build.mk: $(TOPDIR)/src/misc/build.mk
	$(LN) -sf $(TOPDIR)/src/misc/build.mk $@

# Link HALVMCore into GHC's library path, where it will be found and built
# by the GHC build system.
$(TOPDIR)/halvm-ghc/libraries/HALVMCore:
	if [ ! -h $@ ]; then \
	  $(LN) -sf $(TOPDIR)/src/HALVMCore $@ ; \
	fi

# Link XenDevice into GHC's library path, where it will be found and built
# by the GHC build system.
$(TOPDIR)/halvm-ghc/libraries/XenDevice:
	if [ ! -h $@ ]; then \
	  $(LN) -sf $(TOPDIR)/src/XenDevice $@; \
	fi

# Replace libc headers with minlibc
$(TOPDIR)/halvm-ghc/libraries/base/libc-include:
	if [ ! -h $@ ]; then \
	  $(LN) -sf $(TOPDIR)/halvm-ghc/rts/minlibc/include $@ ; \
	fi

GHC_PREPPED = $(TOPDIR)/halvm-ghc/rts/xen/include/xen              \
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
	$(TAR) jxf $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmp-tarballs/*.bz2
	$(INSTALL) -D $(TOPDIR)/halvm-ghc/libraries/integer-gmp/gmp/gmpsrc.patch $(TOPDIR)/gmp-*
	(cd $(TOPDIR)/gmp-* && $(GIT) apply $(TOPDIR)/gmp-*/gmpsrc.patch)
	$(MV) $(TOPDIR)/gmp-* $(TOPDIR)/src/gmp

$(TOPDIR)/src/gmp/Makefile: | $(TOPDIR)/src/gmp
	(cd src/gmp && ABI="$(ABI)" CFLAGS="$(CFLAGS)" \
	    ./configure --disable-shared --enable-static)

$(TOPDIR)/src/gmp/.libs/libgmp.a: $(TOPDIR)/src/gmp/Makefile
	$(MAKE) -C src/gmp

all:: $(TOPDIR)/src/gmp/.libs/libgmp.a

install:: $(TOPDIR)/src/gmp/.libs/libgmp.a
	$(INSTALL) -D $(TOPDIR)/src/gmp/.libs/libgmp.a $(DESTDIR)$(halvmlibdir)/rts-1.0/libgmp.a

clean::
	(cd $(TOPDIR)/halvm-ghc/libraries/integer-gmp && git reset --hard)
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
	              $(DESTDIR)$(halvmlibdir)/rts-1.0/libopenlibm.a

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
	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.a $(DESTDIR)$(libdir)/libIVC.a
	$(INSTALL) -D $(TOPDIR)/src/libIVC/libIVC.h $(DESTDIR)$(incdir)/libIVC.h

###############################################################################
# convert-profile
###############################################################################

$(TOPDIR)/src/profiling/convert-profile: $(TOPDIR)/src/profiling/convert-profile.c
	$(CC) -O2 -o $@ $<

all:: $(TOPDIR)/src/profiling/convert-profile

clean::
	$(RM) -f $(TOPDIR)/src/profiling/convert-profile

install:: $(TOPDIR)/src/profiling/convert-profile
	$(INSTALL) -D $(TOPDIR)/src/profiling/convert-profile $(DESTDIR)$(bindir)/convert-profile

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
	$(INSTALL) -D $(TOPDIR)/src/mkrenddir/mkrenddir $(DESTDIR)$(bindir)/mkrenddir

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
	$(INSTALL) -D $(TOPDIR)/src/bootloader/start.o $(DESTDIR)$(halvmlibdir)/rts-1.0/start.o

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
HALVM_GHC_CONFIGURE_FLAGS += --with-ghc=$(GHC)
HALVM_GHC_CONFIGURE_FLAGS += --prefix=$(prefix)
HALVM_GHC_CONFIGURE_FLAGS += --disable-large-address-space

ifeq ($(INTEGER_LIBRARY),integer-gmp)
HALVM_GHC_CONFIGURE_FLAGS += --with-gmp-includes=$(TOPDIR)/src/gmp
endif

$(TOPDIR)/halvm-ghc/mk/config.mk: $(GHC_PREPPED)
	(cd halvm-ghc && ./configure $(HALVM_GHC_CONFIGURE_FLAGS))

# The GHC build system picks up everything linked into halvm-ghc/libraries
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
	$(MAKE) -C halvm-ghc install ghclibdir=$(halvmlibdir) DESTDIR=$(DESTDIR)
	$(MKDIR) -p $(DESTDIR)$(halvmlibdir)/include/minlibc
	$(CP) -rf halvm-ghc/rts/minlibc/include/* $(DESTDIR)$(halvmlibdir)/include/minlibc
	$(SED) -i -e "s/^extra-ghci-libraries:/extra-ghci-libraries: minlibc/" \
	  $(DESTDIR)$(halvmlibdir)/package.conf.d/base*.conf

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
		 $(TOPDIR)/src/misc/ghci_runtime.o \
		 $(GHCI_OBJS)
	$(AR) cr $@ $(GHCI_OBJS)

all:: $(TOPDIR)/halvm-ghc/libminlibc.a

install:: $(TOPDIR)/halvm-ghc/libminlibc.a
	$(INSTALL) -D $(TOPDIR)/halvm-ghc/libminlibc.a \
	              $(DESTDIR)$(halvmlibdir)/base-$(BASE_VERSION)/libminlibc.a

install:: $(TOPDIR)/src/scripts/halvm-cabal
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-cabal $(DESTDIR)$(bindir)/halvm-cabal

install:: $(TOPDIR)/src/scripts/halvm-config
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-config $(DESTDIR)$(bindir)/halvm-config

install:: $(TOPDIR)/src/scripts/halvm-ghc
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-ghc $(DESTDIR)$(bindir)/halvm-ghc

install:: $(TOPDIR)/src/scripts/halvm-ghc-pkg
	$(INSTALL) -D $(TOPDIR)/src/scripts/halvm-ghc-pkg $(DESTDIR)$(bindir)/halvm-ghc-pkg

install:: $(TOPDIR)/src/scripts/ldkernel
	$(INSTALL) -D $(TOPDIR)/src/scripts/ldkernel $(DESTDIR)$(halvmlibdir)/ldkernel

install:: $(TOPDIR)/src/misc/kernel-$(ARCH).lds
	$(INSTALL) -D $(TOPDIR)/src/misc/kernel-$(ARCH).lds $(DESTDIR)$(halvmlibdir)/kernel.lds

install::
	$(INSTALL) -D $(shell $(GHC) --print-libdir)/bin/hsc2hs $(DESTDIR)${halvmlibdir}/bin/hsc2hs.bin
	$(INSTALL) -D $(shell $(GHC) --print-libdir)/bin/haddock $(DESTDIR)${halvmlibdir}/bin/haddock

# Need to be sure we grab datadirs for alex and happy, /usr/share w.r.t. their prefix
install::
	mkdir -p $(DESTDIR)${halvmlibdir}
	cp ${ALEX} $(DESTDIR)${halvmlibdir}/bin/alex
	cp ${HSCOLOUR} $(DESTDIR)${halvmlibdir}/bin/HsColour
	cp ${CABAL} $(DESTDIR)${halvmlibdir}/bin/cabal
	cp ${HAPPY} $(DESTDIR)${halvmlibdir}/bin/happy
	cp -rf ${platformdir}/share $(DESTDIR)${halvmlibdir}
	cp -rf ${platformdir}/lib $(DESTDIR)${halvmlibdir}
	$(FIND) $(shell $(GHC) --print-libdir) -name "*so" -name '*-ghc*' \
	    -exec cp '{}' $(DESTDIR)$(halvmlibdir)/lib/ \;
	$(INSTALL) -D $(TOPDIR)/src/scripts/hsc2hs $(DESTDIR)${halvmlibdir}/bin/hsc2hs
# (for above)
# hsc2hs requires a bunch of libraries to be installed. This is a hack (FIXME)
# to copy over the platform_ghc ones to our destination directory and hope
# nothing gets broken. Long term, finding some way to build a statically-linked
# hsc2hs would be better.

###############################################################################
# Packaging!
###############################################################################

FILELIST := $(filter-out $(TOPDIR)/HaLVM-$(HaLVM_VERSION),\
              $(filter-out $(TOPDIR)/rpmbuild,\
                $(wildcard $(TOPDIR)/* $(TOPDIR)/.git)))

SRC_TARBALL=HaLVM-$(HaLVM_VERSION).tar.gz

$(SRC_TARBALL):
	rm -rf $(TOPDIR)/HaLVM-${HaLVM_VERSION}
	mkdir -p $(TOPDIR)/HaLVM-${HaLVM_VERSION}
	cp -r $(FILELIST) $(TOPDIR)/HaLVM-${HaLVM_VERSION}/
	tar czf $@ HaLVM-${HaLVM_VERSION}/
	rm -rf $(TOPDIR)/HaLVM-${HaLVM_VERSION}

ifeq ($(PACKAGE_TARGET),RPM)
.PHONY: packages
packages: $(SRC_TARBALL)
	mkdir -p rpmbuild/{SOURCES,SPECS}
	cp $(SRC_TARBALL) $(TOPDIR)/rpmbuild/SOURCES/
	cp $(TOPDIR)/src/misc/HaLVM.spec $(TOPDIR)/rpmbuild/SPECS/HaLVM.spec
	mkdir -p packages
	rpmbuild -ba --define "_topdir $(TOPDIR)/rpmbuild" --define "_version $(HaLVM_VERSION)" --define "_release $(RELEASE)" $(TOPDIR)/rpmbuild/SPECS/HaLVM.spec
	rpmbuild -ba --with gmp --define "_topdir $(TOPDIR)/rpmbuild" --define "_version $(HaLVM_VERSION)" --define "_release $(RELEASE)" $(TOPDIR)/rpmbuild/SPECS/HaLVM.spec
	find rpmbuild -name "*.*rpm" -exec cp '{}' $(TOPDIR)/packages/ \;
endif

ifeq ($(PACKAGE_TARGET),deb)
DEB_ORIGSRC_TARBALL=halvm_$(HaLVM_VERSION).orig.tar.gz
DEB_CONFSRC_TARBALL=halvm_$(HaLVM_VERSION)-$(RELEASE).debian.tar.gz
DEB_DESC_FILE=halvm_$(HaLVM_VERSION)-$(RELEASE).dsc

DEBG_ORIGSRC_TARBALL=halvm-gmp_$(HaLVM_VERSION).orig.tar.gz
DEBG_CONFSRC_TARBALL=halvm-gmp_$(HaLVM_VERSION)-$(RELEASE).debian.tar.gz
DEBG_DESC_FILE=halvm-gmp_$(HaLVM_VERSION)-$(RELEASE).dsc

hash=openssl sha -$1 $2 | sed 's/.*= //g'
sha1=$(call hash,sha1,$1)
sha256=$(call hash,sha256,$1)
size=stat -c "%s" $1

.PHONY: packages
packages: $(DEB_ORIGSRC_TARBALL)  $(DEB_CONFSRC_TARBALL)  $(DEB_DESC_FILE) \
          $(DEBG_ORIGSRC_TARBALL) $(DEBG_CONFSRC_TARBALL) $(DEBG_DESC_FILE)
	rm -rf HaLVM-$(HaLVM_VERSION) halvm-$(HaLVM_VERSION)
	tar zxf $(DEB_ORIGSRC_TARBALL)
	mv HaLVM-$(HaLVM_VERSION) halvm-$(HaLVM_VERSION)
	tar zxf $(DEB_CONFSRC_TARBALL) -C halvm-$(HaLVM_VERSION)/
	(cd halvm-$(HaLVM_VERSION) && dpkg-buildpackage -rfakeroot -uc -us)
	tar zxf $(DEBG_ORIGSRC_TARBALL)
	tar zxf $(DEBG_CONFSRC_TARBALL) -C halvm-gmp-$(HaLVM_VERSION)/
	(cd halvm-gmp-$(HaLVM_VERSION) && dpkg-buildpackage -rfakeroot -uc -us)
	mkdir -p packages
	cp *.deb packages/
	cp *.dsc packages/
	cp *.changes packages/

$(DEB_ORIGSRC_TARBALL): $(SRC_TARBALL)
	cp $(SRC_TARBALL) $(DEB_ORIGSRC_TARBALL)

$(DEB_CONFSRC_TARBALL): $(shell find $(TOPDIR)/src/debian)
	rm -rf tmp
	mkdir tmp
	cp -r src/debian tmp/debian
	sed -ie 's/INTEGER_LIBRARY/simple/g' tmp/debian/rules
	sed -ie 's/VERSION/$(HaLVM_VERSION)/g' tmp/debian/changelog
	sed -ie 's/RELEASE/$(RELEASE)/g' tmp/debian/changelog
	tar cz -C tmp -f $@ debian/

$(DEB_DESC_FILE): $(DEB_ORIGSRC_TARBALL) $(DEB_CONFSRC_TARBALL)
	sed -e 's!ORIG_SHA1!'`$(call sha1,$(DEB_ORIGSRC_TARBALL))`'!g'     \
        -e 's!ORIG_SHA256!'`$(call sha256,$(DEB_ORIGSRC_TARBALL))`'!g' \
        -e 's!CONF_SHA256!'`$(call sha256,$(DEB_CONFSRC_TARBALL))`'!g' \
        -e 's!CONF_SHA1!'`$(call sha1,$(DEB_CONFSRC_TARBALL))`'!g' \
        -e 's!ORIG_SIZE!'`$(call size,$(DEB_ORIGSRC_TARBALL))`'!g' \
        -e 's!CONF_SIZE!'`$(call size,$(DEB_CONFSRC_TARBALL))`'!g' \
        -e 's!VERSION!$(HaLVM_VERSION)!g' \
        -e 's!RELEASE!$(RELEASE)!g' \
        src/misc/halvm.dsc > $(DEB_DESC_FILE)

$(DEBG_ORIGSRC_TARBALL): $(DEB_ORIGSRC_TARBALL)
	rm -rf tmp
	mkdir tmp
	tar zx -C tmp -f $(DEB_ORIGSRC_TARBALL)
	mv tmp/HaLVM-$(HaLVM_VERSION) tmp/halvm-gmp-$(HaLVM_VERSION)
	tar cz -C tmp -f $@ halvm-gmp-$(HaLVM_VERSION)
	rm -rf tmp

$(DEBG_CONFSRC_TARBALL): $(shell find $(TOPDIR)/src/debian)
	rm -rf tmp
	mkdir tmp
	cp -r src/debian tmp/debian
	sed -ie 's/halvm/halvm-gmp/g' tmp/debian/changelog
	sed -ie 's/ halvm/ halvm-gmp/g' tmp/debian/control
	sed -ie 's/INTEGER_LIBRARY/gmp/g' tmp/debian/rules
	sed -ie 's/VERSION/$(HaLVM_VERSION)/g' tmp/debian/changelog
	sed -ie 's/RELEASE/$(RELEASE)/g' tmp/debian/changelog
	tar cz -C tmp -f $@ debian/

$(DEBG_DESC_FILE): $(DEBG_ORIGSRC_TARBALL) $(DEBG_CONFSRC_TARBALL)
	sed -e 's!ORIG_SHA1!'`$(call sha1,$(DEB_ORIGSRC_TARBALL))`'!g'     \
        -e 's!ORIG_SHA256!'`$(call sha256,$(DEB_ORIGSRC_TARBALL))`'!g' \
        -e 's!CONF_SHA256!'`$(call sha256,$(DEB_CONFSRC_TARBALL))`'!g' \
        -e 's!CONF_SHA1!'`$(call sha1,$(DEB_CONFSRC_TARBALL))`'!g' \
        -e 's!ORIG_SIZE!'`$(call size,$(DEB_ORIGSRC_TARBALL))`'!g' \
        -e 's!CONF_SIZE!'`$(call size,$(DEB_CONFSRC_TARBALL))`'!g' \
        -e 's!VERSION!$(HaLVM_VERSION)!g' \
        -e 's!RELEASE!$(RELEASE)!g' \
         -e 's! halvm! halvm-gmp!g' \
        src/misc/halvm.dsc > $(DEB_DESC_FILE)

debclean:
	$(RM) -f $(SRC_TARBALL)
	$(RM) -f $(DEB_ORIGSRC_TARBALL) $(DEBG_ORIGSRC_TARBALL)
	$(RM) -f $(DEB_CONFSRC_TARBALL) $(DEBG_CONFSRC_TARBALL)
	$(RM) -f $(DEB_DESC_FILE) $(DEBG_DESC_FILE)
	$(RM) -rf HaLVM-$(HaLVM_VERSION)
	$(RM) -rf halvm-$(HaLVM_VERSION)
	$(RM) -rf halvm-gmp-$(HaLVM_VERSION)
	$(RM) -f *.deb *.changes

clean:: debclean
endif #ifeq deb
