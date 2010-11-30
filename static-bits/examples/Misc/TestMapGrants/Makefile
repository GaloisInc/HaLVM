# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#
all: Sender Receiver

dist/setup-config: TestMapGrants.cabal
	halvm-cabal configure

Sender: Sender.hs dist/setup-config
	halvm-cabal build

Receiver: Receiver.hs dist/setup-config
	halvm-cabal build

run: Sender Receiver
	-$(SUDO) xm destroy Sender
	-$(SUDO) xm destroy Receiver
	$(SUDO) xm dmesg --clear > /dev/null
	$(SUDO) xm create Receiver.config
	$(SUDO) xm create Sender.config
	sleep 3
	$(SUDO) xm dmesg

clean:
	rm -rf Sender Receiver *.o *.hi dist
	-$(SUDO) xm destroy Sender
	-$(SUDO) xm destroy Receiver
