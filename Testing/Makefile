# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Magnus Carlsson <magnus@galois.com>
# BANNEREND
PLATFORMGHC=../platform_ghc/bin/ghc

runTest: runTest.hs POpen.hs Interactions.hs
	$(PLATFORMGHC) --make runTest.hs -i..

clean:
	rm -f *.o *.hi runTest
