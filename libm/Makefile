include ../mk/common.mk

CFLAGS   = -Wall -Werror -Wno-unused-variable -O2 -fno-builtin -nostdinc
CFLAGS  += -I$(TOPDIR)/ghc-xen-sparse/rts/xen/include
ASFLAGS  = -Wall -Werror -O2 -fno-builtin -nostdinc
ASFLAGS += -I$(TOPDIR)/ghc-xen-sparse/rts/xen/include

C_FILES = $(shell find . -name '*.c')
S_FILES = $(shell find . -name '*.S')
O_FILES = $(C_FILES:.c=.o) $(S_FILES:.S=.o)

libm.a: $(O_FILES)
	$(AR) rcs libm.a $(O_FILES)

clean:
	$(RM) -f $(O_FILES) libm.a
