# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#
all: Hello

Hello: Hello.hs
	halvm-ghc --make -o Hello Hello.hs

run: Hello
	sudo xm dmesg -c > /dev/null
	sudo xm create Hello.config -c
	sudo xm dmesg

clean:
	rm -f Hello.{hi,o} Hello
