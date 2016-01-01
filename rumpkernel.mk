ifeq ($(ARCH),x86_64)
RUMP_ARCH=amd64
else
RUMP_ARCH=i686
endif

RUMP_INCLUDE_DIR  := $(TOPDIR)/rumprun/obj-$(RUMP_ARCH)-xen/dest.stage/include
RUMPKERNEL_TARGET := $(RUMP_INCLUDE_DIR)/math.h

$(RUMPKERNEL_TARGET):
	(cd rumprun && ./build-rr.sh xen build)
