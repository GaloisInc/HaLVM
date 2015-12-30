################################################################################
#
# File Downloading
#
################################################################################

TARBALL_DIR   := $(TOPDIR)/tarballs
GHC_TARBALL   := $(TARBALL_DIR)/$(GHC_FILE)
CABAL_TARBALL := $(TARBALL_DIR)/$(CABAL_FILE)

$(GHC_TARBALL): | $(TARBALL_DIR)
	(cd $(TARBALL_DIR) && $(CURL) -LO $(GHC_LINK))

$(CABAL_TARBALL): | $(TARBALL_DIR)
	(cd $(TARBALL_DIR) && $(CURL) -LO $(CABAL_LINK))

$(TARBALL_DIR):
	$(MKDIR) -p $(TARBALL_DIR)

drproper::
	$(RM) -rf $(TARBALL_DIR)

################################################################################
#
# Platform GHC Installation
#
################################################################################

PLATFORM_GHC_DIR  := $(TOPDIR)/platform-ghc
PLATFORM_GHC      := $(PLATFORM_GHC_DIR)/bin/ghc
PLATFORM_CABAL    := $(PLATFORM_GHC_DIR)/bin/cabal
PLATCABAL         := $(PLATFORM_GHC_DIR)/bin/cabal.sh
PLATALEX 	  := $(PLATFORM_GHC_DIR)/bin/alex
PLATHAPPY	  := $(PLATFORM_GHC_DIR)/bin/happy
PLATHADDOCK       := $(PLATFORM_GHC_DIR)/bin/haddock
PLATHSCOLOUR      := $(PLATFORM_GHC_DIR)/bin/HsColour

$(PLATFORM_GHC_DIR):
	$(MKDIR) -p $(PLATFORM_GHC_DIR)

$(PLATFORM_GHC): $(GHC_TARBALL) | $(PLATFORM_GHC_DIR) $(PLATFORM_GHC_DIR)
	$(TAR) jxf $(GHC_TARBALL) -C $(PLATFORM_GHC_DIR)
	(cd $(PLATFORM_GHC_DIR)/ghc* && ./configure --prefix=$(PLATFORM_GHC_DIR))
	$(MAKE) -C $(PLATFORM_GHC_DIR)/ghc*/ install
	$(RM) -rf $(PLATFORM_GHC_DIR)/ghc*

$(PLATFORM_CABAL): $(PLATFORM_GHC) $(CABAL_TARBALL) | $(PLATFORM_GHC_DIR)
	$(TAR) zxf $(CABAL_TARBALL) -C $(PLATFORM_GHC_DIR)
	(cd $(PLATFORM_GHC_DIR)/cabal* && PREFIX=${halvmlibdir} PATH=${PATH}:${PLATFORM_GHC_DIR}/bin ./bootstrap.sh --no-doc --sandbox $(PLATFORM_GHC_DIR))

$(PLATFORM_GHC_DIR)/cabal.config: $(PLATFORM_CABAL)
	echo "require-sandbox: True" >> $@
	echo "remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive" >> $@
	echo "remote-repo-cache: $(PLATFORM_GHC_DIR)/packages" >> $@

$(PLATFORM_GHC_DIR)/cabal.sandbox.config: $(PLATFORM_CABAL) $(PLATFORM_GHC_DIR)/cabal.config
	(cd $(PLATFORM_GHC_DIR) && PATH=$(PATH):$(PLATFORM_GHC_DIR)/bin $(PLATFORM_CABAL) sandbox init --sandbox=.)

$(PLATCABAL): $(PLATFORM_CABAL) $(PLATFORM_GHC_DIR)/cabal.config $(PLATFORM_GHC_DIR)/cabal.sandbox.config
	echo -n "PATH=${PATH}:$(PLATFORM_GHC_DIR)/bin " > $@
	echo -n "$(PLATFORM_CABAL) --config-file=$(PLATFORM_GHC_DIR)/cabal.config " >> $@
	echo    "\$${1+\"\$$@\"}" >> $@
	chmod +x $@
	(cd $(PLATFORM_GHC_DIR) && $(PLATCABAL) update)

$(PLATALEX): $(PLATCABAL)
	(cd $(PLATFORM_GHC_DIR) && $(PLATCABAL) install alex)

$(PLATHAPPY): $(PLATCABAL) $(PLATALEX)
	(cd $(PLATFORM_GHC_DIR) && $(PLATCABAL) install happy)

$(PLATHADDOCK): $(PLATCABAL) $(PLATHSCOLOUR)
	(cd $(PLATFORM_GHC_DIR) && $(PLATCABAL) install haddock)

$(PLATHSCOLOUR): $(PLATCABAL)
	(cd $(PLATFORM_GHC_DIR) && $(PLATCABAL) install hscolour)

.PHONY: platform-tools
platform-tools: $(PLATCABAL) $(PLATALEX) $(PLATHAPPY) $(PLATHADDOCK) $(PLATHSCOLOUR)

mrproper::
	$(RM) -rf $(PLATFORM_GHC_DIR)

