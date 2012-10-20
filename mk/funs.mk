define build_downloader
$$($(1)_TARBALL):
	@mkdir -p tarballs
	wget $$($(1)_LINK) -O $$($(1)_TARBALL)
	$(TOUCH) $$($(1)_TARBALL)

mrproper::
	$(RM) $$($(1)_TARBALL)
endef

hackage_tarball = $(TOPDIR)/tarballs/$1-$2.tar.gz
hackage_link    = $(HACKAGE)/$1/$2/$1-$2.tar.gz
hackage_target  = $(TOPDIR)/platform_ghc/lib/$1-$2/ghc-$(GHC_VER)/libHS$(1)-$(2).a

define build_platcabal_lib_stuff
$(1)_TARGET  := $(call hackage_target,$1,$($(1)_VERSION))
$(1)_LINK    := $(call hackage_link,$1,$($(1)_VERSION))
$(1)_TARBALL := $(call hackage_tarball,$1,$($(1)_VERSION))

$(call build_downloader,$1)

$$($(1)_TARGET): $(PLATFORM_GHC) $$($(1)_TARBALL)
	$$(call build_cabal_target,$$($(1)_TARBALL),$1,$$($(1)_VERSION))
endef
