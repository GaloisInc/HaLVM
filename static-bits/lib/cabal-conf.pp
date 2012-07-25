remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive
remote-repo-cache: CABAL_CACHE_DIR/packages

-- local-repo:
documentation: BUILD_DOCS
-- root-cmd:
-- symlink-bindir:
-- cabal-lib-version:
-- log-builds: False
-- build-reports:
-- verbose: 1
-- distpref: dist
compiler: ghc
with-compiler: GHC_COMPILER
with-hc-pkg: GHC_PKG
#ifdef INCLUDE_AUX_PROGRAMS
with-haddock: HADDOCK_PROG
haddock-opts: HADDOCK_OPTIONS
with-hsc2hs: HSC2HS_PROG
with-alex: ALEX_PROG
with-happy: HAPPY_PROG
with-hscolour: HSCOLOUR_PROG
ghc-opts: GHC_OPTIONS
#endif
-- program-prefix: 
-- program-suffix: 
library-vanilla: True
library-profiling: False
shared: False
executable-profiling: False
-- optimization:
library-for-ghci: True
#ifdef SPLIT_OBJS
split-objs: True
#endif
executable-stripping: True
user-install: False
-- package-db:
-- flags:
#ifdef EXTRA_INCDIRS
extra-include-dirs: EXTRA_INCDIRS
extra-include-dirs: EXTRA_INCDIRS/xenghc
extra-include-dirs: EXTRA_INCDIRS/xenghc/sys
#endif
#ifdef EXTRA_LIBDIRS
extra-lib-dirs: EXTRA_LIBDIRS
#endif
-- constraint:
-- username:
-- password:

install-dirs global
  prefix: INSTALL_PREFIX
  bindir: $prefix/bin
  libdir: $prefix/lib/HaLVM-HALVM_VER
  libsubdir: $pkgid/
  libexecdir: $prefix/libexec
  datadir: $prefix/share
  datasubdir: $pkgid
  docdir: $datadir/doc/HaLVM-HALVM_VER/$pkgid
  htmldir: $docdir/html
  haddockdir: $htmldir

install-dirs user
