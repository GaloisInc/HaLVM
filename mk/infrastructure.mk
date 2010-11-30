# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND

include mk/common.mk

PLATFORM_TOOLS	= $(PLATFORM_GHC) $(PLATFORM_CABAL_EXE)			\
		  $(PLATFORM_HADDOCK) $(PLATFORM_ALEX)			\
		  $(PLATFORM_HAPPY) $(PLATFORM_HSCOLOUR)

.PHONY:
all: $(PLATFORM_TOOLS) $(GHC_XENDIR_TARGET)

.PHONY:
clean:
	$(RM) -rf platform_ghc dist xen-ghc
	$(RM) -rf $(NEW_GHC_LIB_LOCS)
	$(RM) -rf $(TOPDIR)/libraries/BoundedChan/BoundedChan-$(BC_VER)

.PHONY:
mrproper:
	$(RM) -rf tarballs

#
#
#

$(GHC_XENDIR_TARGET): $(PLATFORM_TOOLS) $(GHC_SRC_TARBALL)
	$(RM) -rf tmp xen-ghc
	$(MKDIR) tmp
	$(TAR) jxf $(GHC_SRC_TARBALL) -C tmp
	$(MV) tmp/ghc-$(GHC_VER) xen-ghc
	$(RM) -rf tmp
	@$(ECHO) "--- BOOTING GHC ---"
	(cd xen-ghc; perl boot )
	@$(ECHO) "--- PATCHING GHC ---"
	(cd xen-ghc; for p in ../patches/ghc/*; do $(PATCH) -p1 < $$p; done)
	@$(ECHO) "--- TRANSFERRING GHC/Xen FILES ---"
	 (cd ghc-xen-sparse &&						\
	  $(FIND) * -type d -exec $(MKDIR) -p ../xen-ghc/'{}' \; )
	( cd ghc-xen-sparse &&						\
	  $(FIND) * -type f -exec $(LN) -sf `pwd`/'{}' ../xen-ghc/'{}' \; )
	$(ECHO) "SRC_HC_OPTS = -H32m -O2 -optc$(NO_STACK_PROTECTOR_OPT) $(SANITY_CHECKER_OPT) $(BYTECODE_INTERP_OPT) $(GHC_ARCH_OPT) -I$(TOPDIR)/xen-ghc/rts/xen/include -I$(TOPDIR)/xen-ghc/rts/xen/include/sys"> $@
	$(ECHO) "SRC_CC_OPTS = $(CFLAGS)" >> $@
	test -n $(USE_GMP) && $(ECHO) "INTEGER_LIBRARY = $(INTEGER_LIBRARY)" >> $@
	$(SED) -e 's!@ProjectVersion@!$(GHC_VER)!g' xen-ghc/compiler/ghc.cabal.in \
	  > xen-ghc/compiler/ghc.cabal

$(PLATFORM_GHC): $(GHC_BINARY_TARBALL)
	$(RM) -rf tmp
	$(MKDIR) tmp
	$(TAR) jxf $(GHC_BINARY_TARBALL) -C tmp
	( cd tmp/ghc* && ./configure --prefix=$(PLATFORM_GHC_PATH) )
	( cd tmp/ghc* && $(MAKE) -j1 install )
	$(RM) -rf tmp

$(PLATFORM_HADDOCK): $(PLATFORM_ALEX) $(PLATFORM_HAPPY) $(PLATFORM_CABAL_EXE)
	$(call build_cabalinst_target,ghc-paths,$(GHC_PATHS_VERSION))
	$(call build_cabalinst_target,haddock,$(HADDOCK_VERSION))

$(PLATFORM_ALEX): $(PLATFORM_CABAL_EXE)
	$(call build_cabalinst_target,alex,$(ALEX_VERSION))

$(PLATFORM_HAPPY): $(PLATFORM_CABAL_EXE)
	$(call build_cabalinst_target,happy,$(HAPPY_VERSION))

$(PLATFORM_HSCOLOUR): $(PLATFORM_CABAL_EXE)
	$(call build_cabalinst_target,hscolour,$(HSCOLOUR_VERSION))

$(PLATFORM_CABAL_EXE): $(PLATFORM_GHC) $(CABAL_INST_TARBALL)		\
		       $(ZLIB_TARGET) $(HTTP_TARGET) $(CABAL_TARGET)
	$(call build_cabal_target,$(CABAL_INST_TARBALL),cabal-install,$(CI_VER))
	$(MKDIR) -p $(PLATFORM_GHC_PATH)/.cabal
	$(CPP) -std=c89							\
	       -DCABAL_CACHE_DIR=$(PLATFORM_GHC_PATH)/.cabal		\
	       -DBUILD_DOCS=False					\
	       -DGHC_COMPILER=$(PLATFORM_GHC)				\
	       -DGHC_PKG=$(PLATFORM_GHC_PKG)				\
	       -DINSTALL_PREFIX=$(PLATFORM_GHC_PATH)			\
	       -DHALVM_VER=$(HALVM_VER)					\
	     static-bits/lib/cabal.conf					\
	  | $(SED) "/^#.*/d"						\
	  > $(PLATFORM_GHC_PATH)/cabal_config
	$(PLATFORM_CABAL) update

$(ZLIB_TARGET): $(ZLIB_TARBALL)
	$(call build_cabal_target,$(ZLIB_TARBALL),zlib,$(ZLIB_VERSION))

$(HTTP_TARGET): $(HTTP_TARBALL) $(NETWORK_TARGET)
	$(call build_cabal_target,$(HTTP_TARBALL),HTTP,$(HTTP_VERSION))

$(CABAL_TARGET): $(CABAL_TARBALL)
	$(call build_cabal_target,$(CABAL_TARBALL),Cabal,$(CABAL_VERSION))
	( SIG=`$(FIND) $(PLATFORM_PACKAGE_D) -name 'Cabal-*' \
	       | sed 's/^.*\([a-z0-9]\{32\}\).*$$/\1/'`; \
	  DEPS=`$(FIND) $(PLATFORM_PACKAGE_D) \
	        -exec grep -q Cabal '{}' \; -print`; \
	  for pkg in $$DEPS; do \
	    $(SED) "s/$(CABAL_ORIG_SIG)/$$SIG/" -i $$pkg; \
	  done; \
	  $(PLATFORM_GHC_PKG) recache; \
	)

$(NETWORK_TARGET): $(NETWORK_TARBALL) $(PARSEC_TARGET)
	$(call build_cabal_target,$(NETWORK_TARBALL),network,$(NETWORK_VERSION))

$(PARSEC_TARGET): $(PARSEC_TARBALL) $(MTL_TARGET)
	$(call build_cabal_target,$(PARSEC_TARBALL),parsec,$(PARSEC_VERSION))

$(MTL_TARGET): $(MTL_TARBALL)
	$(call build_cabal_target,$(MTL_TARBALL),mtl,$(MTL_VERSION))

#
# wget the tarballs
#

TARBALL_DIR_STAMP = tarballs/.stamp

$(TARBALL_DIR_STAMP):
	$(MKDIR) -p tarballs
	$(TOUCH) $(TARBALL_DIR_STAMP)

$(GHC_BINARY_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(GHC_BINARY_LINK) -O $(GHC_BINARY_TARBALL)
	$(TOUCH) $(GHC_BINARY_TARBALL)

$(GHC_SRC_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(GHC_SRC_LINK) -O $(GHC_SRC_TARBALL)
	$(TOUCH) $(GHC_SRC_TARBALL)

$(GHC_EXTRAS_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(GHC_EXTRAS_LINK) -O $(GHC_EXTRAS_TARBALL)
	$(TOUCH) $(GHC_EXTRAS_TARBALL)

$(CABAL_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(CABAL_LINK) -O $(CABAL_TARBALL)
	$(TOUCH) $(CABAL_TARBALL)

$(ZLIB_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(ZLIB_LINK) -O $(ZLIB_TARBALL)
	$(TOUCH) $(ZLIB_TARBALL)

$(HTTP_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(HTTP_LINK) -O $(HTTP_TARBALL)
	$(TOUCH) $(HTTP_TARBALL)

$(CABAL_INST_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(CABAL_INST_LINK) -O $(CABAL_INST_TARBALL)
	$(TOUCH) $(CABAL_INST_TARBALL)

$(FGL_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(FGL_LINK) -O $(FGL_TARBALL)
	$(TOUCH) $(FGL_TARBALL)

$(TIME_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(TIME_LINK) -O $(TIME_TARBALL)
	$(TOUCH) $(TIME_TARBALL)

$(BC_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(BC_LINK) -O $(BC_TARBALL)
	$(TOUCH) $(BC_TARBALL)

$(PARSEC_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(PARSEC_LINK) -O $(PARSEC_TARBALL)
	$(TOUCH) $(PARSEC_TARBALL)

$(MTL_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(MTL_LINK) -O $(MTL_TARBALL)
	$(TOUCH) $(MTL_TARBALL)

$(NETWORK_TARBALL): $(TARBALL_DIR_STAMP)
	wget $(NETWORK_LINK) -O $(NETWORK_TARBALL)
	$(TOUCH) $(NETWORK_TARBALL)
