remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive
remote-repo-cache: CABAL_CACHE_DIR/packages
-- local-repo:
-- logs-dir:
world-file: CABAL_CACHE_DIR/world
-- verbose: 1
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
-- scratchdir:
-- program-prefix: 
-- program-suffix: 
library-vanilla: True
library-profiling: False
shared: False
executable-dynamic: False
executable-profiling: False
-- optimization: True
-- library-for-ghci: True
#ifdef SPLIT_OBJS
split-objs: True
#endif
-- split-objs: False
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
tests: False
library-coverage: False
benchmarks: False
-- cabal-lib-version:
-- constraint:
-- preference:
-- solver: choose
documentation: BUILD_DOCS
-- doc-index-file: $datadir/doc/index.html
-- max-backjumps: 200
-- reorder-goals: False
-- shadow-installed-packages:
-- reinstall: False
-- avoid-reinstalls: False
-- force-reinstalls: False
-- upgrade-dependencies: False
-- only-dependencies: False
-- root-cmd:
-- symlink-bindir:
build-summary: CABAL_CACHE_DIR/logs/build.log
-- build-log:
remote-build-reporting: anonymous
-- one-shot: False
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
