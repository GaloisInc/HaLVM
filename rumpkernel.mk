ifeq ($(ARCH),x86_64)
RUMP_ARCH=amd64
else
RUMP_ARCH=i686
endif

RUMP_STAGE_DIR    := $(TOPDIR)/rumprun/obj-$(RUMP_ARCH)-xen/dest.stage
RUMP_STAGE_LIBDIR := $(RUMP_STAGE_DIR)/rumprun-$(ARCH)/lib
RUMP_INCLUDE_DIR  := $(RUMP_STAGE_DIR)/include
RUMPKERNEL_TARGET := $(RUMP_INCLUDE_DIR)/math.h

$(RUMPKERNEL_TARGET):
	(cd rumprun && ./build-rr.sh -d $(halvmlibdir)/rump xen build)

install:: $(RUMPKERNEL_TARGET)
	(cd rumprun && ./build-rr.sh -d $(halvmlibdir)/rump xen install)

install:: $(RUMPKERNEL_TARGET)
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/xen.ldscript $(halvmlibdir)/xen.ldscript
