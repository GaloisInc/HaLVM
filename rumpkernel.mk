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
	(cd rumprun && ./build-rr.sh xen build)

install:: $(RUMPKERNEL_TARGET)
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/libc.a $(halvmlibdir)/rts/libc.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/libm.a $(halvmlibdir)/rts/libm.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/libpthread.a $(halvmlibdir)/rts/libpthread.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librump.a $(halvmlibdir)/rts/librump.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumprun_base.a $(halvmlibdir)/rts/librumprun_base.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/rumprun.o $(halvmlibdir)/rts/rumprun.o
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumprun_base.a $(halvmlibdir)/rts/librumprun_base.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpvfs.a $(halvmlibdir)/rts/librumpvfs.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpkern_bmktc.a $(halvmlibdir)/rts/librumpkern_bmktc.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpdev.a $(halvmlibdir)/rts/librumpdev.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpfs_tmpfs.a $(halvmlibdir)/rts/librumpfs_tmpfs.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpnet_config.a $(halvmlibdir)/rts/librumpnet_config.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpnet_net.a $(halvmlibdir)/rts/librumpnet_net.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpnet.a $(halvmlibdir)/rts/librumpnet.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpdev_bpf.a $(halvmlibdir)/rts/librumpdev_bpf.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpdev_vnd.a $(halvmlibdir)/rts/librumpdev_vnd.a
	$(INSTALL) -D $(RUMP_STAGE_LIBDIR)/rumprun-xen/librumpdev_rnd.a $(halvmlibdir)/rts/librumpdev_rnd.a
