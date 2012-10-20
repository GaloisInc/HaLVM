# This file is going away. You are on an interim commit. Please move to HEAD
# or wait for this branch to be merged.

(GHC_XENDIR_TARGET): $(PLATFORM_TOOLS) $(GHC_SRC_TARBALL)
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
	$(ECHO) "SRC_HC_OPTS = -H32m -O2 -optc$(NO_STACK_PROTECTOR_OPT) $(SANITY_CHECKER_OPT) $(BYTECODE_INTERP_OPT) $(GHC_ARCH_OPT) $(GHC_ARCH_FLAGS) -I$(TOPDIR)/xen-ghc/rts/xen/include -I$(TOPDIR)/xen-ghc/rts/xen/include/sys"> $@
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
