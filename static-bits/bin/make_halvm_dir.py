#!/usr/bin/python
#
# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#
import sys

sys.path.append("/usr/lib/python")
from xen.xend.xenstore.xstransact import xstransact

xstransact.Remove("/halvm")
xstransact.Mkdir("/halvm")
xstransact.SetPermissions("/halvm", { 'dom' : 0, 'read' : True, 'write' : True })

