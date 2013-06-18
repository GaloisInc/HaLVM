HALVM_LIBDIR := $(shell halvm-ghc --print-libdir)

all: $(BINARIES)

clean::
	rm -f $(BINARIES) *.hi *.o

%: %.hs
	halvm-ghc --make -o $@ $^

%: c/%.c
	gcc -o $@ $^ -I$(HALVM_LIBDIR)/include -lxenctrl -lcrypto -lxenstore $(HALVM_LIBDIR)/libIVC.a
