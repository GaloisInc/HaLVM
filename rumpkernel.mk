RUMPKERNEL_TARGET := $(TOPDIR)/rumpkernel/obj/libz.a

$(RUMPKERNEL_TARGET):
	(cd rumpkernel && ./buildrump.sh checkout tools build)
