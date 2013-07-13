
all: rts/start.o

rts/start.o: ASFLAGS += -I$(TOPDIR)/halvm-ghc/rts/xen/include
rts/start.o: rts/start.$(ARCH).S
	$(call cmd,cc_o_S)

install: $(halvm-dir)/start.o

$(halvm-dir)/start.o: rts/start.o
	$(call cmd,install)

clean::
	$(call label,clean,rts)
