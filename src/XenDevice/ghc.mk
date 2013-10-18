libraries/XenDevice_PACKAGE = XenDevice
libraries/XenDevice_dist-install_GROUP = libraries
$(if $(filter XenDevice,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/XenDevice,dist-boot,0)))
$(if $(filter XenDevice,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/XenDevice,dist-install,1)))
$(if $(filter XenDevice,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/XenDevice,dist-install,2)))
