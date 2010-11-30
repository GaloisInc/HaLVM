# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#

# This Makefile just forwards off to the other two.
#

all:
	$(MAKE) -f mk/infrastructure.mk all
	$(MAKE) -f mk/halvm.mk all

docs:
	$(MAKE) -f mk/infrastructure.mk all
	$(MAKE) -f mk/halvm.mk docs

infrastructure:
	$(MAKE) -f mk/infrastructure.mk all
	$(MAKE) -f mk/tests.mk all

examples:
	$(MAKE) -f mk/tests.mk examples

tests: infrastructure
	$(MAKE) -f mk/tests.mk tests

clean:
	$(MAKE) -f mk/halvm.mk clean
	$(MAKE) -f mk/infrastructure.mk clean

partial-clean:
	$(MAKE) -f mk/halvm.mk partial-clean

mrproper:
	$(MAKE) -f mk/halvm.mk mrproper
	$(MAKE) -f mk/infrastructure.mk mrproper
	$(MAKE) -f mk/tests.mk clean

remove-%:
	$(MAKE) -f mk/halvm.mk $@

install:
	$(MAKE) -f mk/halvm.mk install
