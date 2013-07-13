# BANNERSTART
# - Copyright 2006-2008,2013 Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND

SUBDIRS = $(dir $(wildcard */*/Makefile))

.PHONY: all clean

all::

clean::

define build_rule
all::
	@$(MAKE) -C $1 all

clean::
	@$(MAKE) -C $1 clean
endef

$(foreach d,$(SUBDIRS),$(eval $(call build_rule,$(d))))

