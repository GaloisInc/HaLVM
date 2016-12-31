Change Log
======

Please document any notables changes you made here.

## v2.3.0

In the 2.3.0 release, HaLVM will be upgraded to
use GHC 8.0.1. So all related changes are made towards this direction.

Also, this is the first time we have a `CHANGELOG.md`!

Unless otherwise stated, all changed in this section are made by Zhen Zhang ([@izgzhen](https://github.com/izgzhen)).

### Added
+ `CHANGELOG.md`

### Updated
+ `halvm-ghc`: Tracking a modified GHC 8.0.1
+ `HACKING.md`
+ `Makefile`
    - Deleted all manual stage0 compiler downloading and configuration process
    - Deleted the `halvm-base` clone
    - Added `--disable-large-address-space` GHC-8 configure flag
    - Commented out "hsc2hs requires a bunch of libraries to be installed" part
+ `autoconf.mk.in`
    - Deleted all manual stage0 compiler configuration
+ `configure.ac`
    - Updated version number
+ `src/misc/build.mk.in`
    - Updated to GHC-8 build config, based mainly on `quick-cross` flavour
+ `src/scripts/ldkernel.in`
    - Update the way to handle arguments provided by new compiler
