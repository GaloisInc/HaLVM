libraries/HALVMCore_PACKAGE = HALVMCore
libraries/HALVMCore_dist-install_GROUP = libraries
$(if $(filter HALVMCore,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/HALVMCore,dist-boot,0)))
$(if $(filter HALVMCore,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/HALVMCore,dist-install,1)))
$(if $(filter HALVMCore,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/HALVMCore,dist-install,2)))
