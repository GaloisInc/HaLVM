# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Iavor S. Diatchki <diatchki@galois.com>
# BANNEREND

# To use, define:
#   PLATFORM_GHC: the platform compiler (not the Xen one) [has default]
#   PACKAGE:      the name of the package that is being built [has default]
#   EXTRA_DEPS:   extra dependecies

TOP:=$(shell halvmc --print-halvmdir)
ifeq ($(TOP),)
TOP := $(shell cd ../dist && pwd)
endif
PLATFORM_GHC ?= $(TOP)/../platform_ghc/bin/ghc
PLATFORM_HADDOCK ?= $(TOP)/../platform_ghc/bin/haddock
PACKAGE ?= $(basename *.cabal)

default :: ./setup/Setup $(PACKAGE).cabal $(EXTRA_DEPS)
	./setup/Setup configure \
		--prefix=$(TOP) \
		--docdir=$(TOP)/doc/libs \
		--with-compiler=$(TOP)/bin/xen-ghc \
		--with-hc-pkg=$(TOP)/bin/xen-ghc-pkg \
		--with-hsc2hs=$(TOP)/bin/hsc2hs \
		--enable-split-objs \
		--with-haddock=$(PLATFORM_HADDOCK) \
		--haddock-options="--use-contents=$(TOP)/doc/index.html --use-index=$(TOP)/doc/doc-index.html" \
		--hsc2hs-options="-c $(PLATFORM_GHC)"

	./setup/Setup build
	./setup/Setup haddock
	if [ -f cbits/ghci_stubs.c ]; then \
	  $(CC) -c -o dist/ghci_stubs.o $(CFLAGS) $(CPPFLAGS) cbits/ghci_stubs.c &&\
	  $(LD) -r -o dist/ghci_file.o dist/ghci_stubs.o \
	    `find dist/build -name '*.o' \! -name '*__*' \! -name 'HS*'` && \
      cp dist/ghci_file.o dist/build/HS$(PACKAGE)*.o; \
	fi
	./setup/Setup install

$(PACKAGE).buildinfo: $(PACKAGE).buildinfo.in
	echo "Please reconfigure the package by running 'configure'"
	false

setup/Setup: Setup.lhs
	mkdir -p setup
	$(PLATFORM_GHC) --make -hidir setup -odir setup -o setup/Setup Setup.lhs


clean:
	if [ -f ./setup/Setup ]; then ./setup/Setup clean; fi
	rm -rf ./setup
