# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

# THE HALVM BUILD PROCESS:
#
# 1. Download and install a version of GHC that runs on the current platform.
# 2. Add libraries to the platform version of GHC until it is capable of
#    running cabal-install.
# 3. Use that platform cabal-install to install the HaLVM tools that will run on
#    the platform: haddock, happy, alex, etc..
# 4. Download the GHC source and patch it appropriately for running on Xen.
# 5. Use the platform GHC to build the patched GHC compiler, creating the
#    HaLVM compiler.
# 6. Use the HaLVM compiler to build the core GHC libraries.
# 7. Use the HaLVM compiler and core libraries to build the core HaLVM
#    libraries.
# 8. Copy/install everything into a useful place.
#

include mk/common.mk
include mk/funs.mk

all: $(PLATFORM_CABAL_EXE)

###############################################################################
###############################################################################
#
# STEP #1: Downaload and install a version of GHC that runs on the current
#          platform.
#
###############################################################################
###############################################################################

$(PLATFORM_GHC): $(GHC_BINARY_TARBALL)
	$(RM) -rf tmp
	$(MKDIR) tmp
	$(TAR) jxf $(GHC_BINARY_TARBALL) -C tmp
	( cd tmp/ghc-$(GHC_VER) && ./configure --prefix=$(PLATFORM_GHC_PATH) )
	( cd tmp/ghc-$(GHC_VER) && $(MAKE) -j1 install )
	$(RM) -rf tmp

$(eval $(call build_downloader,GHC_BINARY))

clean::
	$(RM) -rf platform_ghc

###############################################################################
###############################################################################
#
# STEP #2: Add libraries to the platform version of GHC until it is capable of
#          running cabal-install.
#
###############################################################################
###############################################################################

STEP2_LIBRARIES = transformers mtl network text parsec Cabal zlib HTTP random

$(foreach l,$(STEP2_LIBRARIES),$(eval $(call build_platcabal_lib_stuff,$l)))

$(mtl_TARGET): $(transformers_TARGET)
$(parsec_TARGET): $(text_TARGET)
$(network_TARGET): $(parsec_TARGET)
$(HTTP_TARGET): $(network_TARGET)
$(HTTP_TARGET): $(mtl_TARGET)
$(HTTP_TARGET): $(parsec_TARGET)

$(PLATFORM_CABAL_EXE): $(Cabal_TARGET)
$(PLATFORM_CABAL_EXE): $(mtl_TARGET)
$(PLATFORM_CABAL_EXE): $(HTTP_TARGET)
$(PLATFORM_CABAL_EXE): $(network_TARGET)
$(PLATFORM_CABAL_EXE): $(zlib_TARGET)
$(PLATFORM_CABAL_EXE): $(random_TARGET)
$(PLATFORM_CABAL_EXE): $(PLATFORM_GHC)

$(PLATFORM_CABAL_EXE): $(CABAL_INST_TARBALL)
	$(call build_cabal_target,$(CABAL_INST_TARBALL),cabal-install,$(CI_VER))
	$(MKDIR) -p $(PLATFORM_GHC_PATH)/.cabal
	$(CPP) -std=c89							\
	       -DCABAL_CACHE_DIR=$(PLATFORM_GHC_PATH)/.cabal		\
	       -DBUILD_DOCS=False					\
	       -DGHC_COMPILER=$(PLATFORM_GHC)				\
	       -DGHC_PKG=$(PLATFORM_GHC_PKG)				\
	       -DINSTALL_PREFIX=$(PLATFORM_GHC_PATH)			\
	       -DHALVM_VER=$(HALVM_VER)					\
	     static-bits/lib/cabal-conf.pp					\
	  | $(SED) "/^#.*/d"						\
	  > $(PLATFORM_GHC_PATH)/cabal_config
	$(PLATFORM_CABAL) update

$(eval $(call build_downloader,CABAL_INST))

###############################################################################
###############################################################################
#
# STEP #3. Use that platform cabal-install to install the HaLVM tools that will
#          run on the platform: haddock, happy, alex, etc..
#
###############################################################################
###############################################################################

HALVM_TOOLS        = haddock happy alex hscolour
HALVM_TOOL_TARGETS = $(foreach t,$(HALVM_TOOLS),$(PLATFORM_BIN_PATH)/$t)

$(foreach t,$(HALVM_TOOLS),$(eval $(call build_cabalinst_target,$t)))

$(PLATFORM_BIN_PATH)/haddock: $(PLATFORM_BIN_PATH)/alex
$(PLATFORM_BIN_PATH)/haddock: $(PLATFORM_BIN_PATH)/happy

all: $(HALVM_TOOL_TARGETS)
