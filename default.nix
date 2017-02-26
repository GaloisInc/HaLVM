{ pkgs ? import <nixpkgs> {}, useGMP ? true }:

pkgs.stdenv.mkDerivation {
  name = "halvm";
  src = ./.;
  prePatch = ''
    sed -i '312 d' Makefile
  '';
  buildInputs =
   let haskellPkgs =
    with pkgs.haskell.packages.ghc802; [
      alex happy hscolour cabal-install haddock
    ]; in with pkgs; [
       haskell.compiler.ghc802
       automake perl git binutils
       autoconf xen zlib ncurses.dev
       libtool gmp ] ++ haskellPkgs;
  hardeningDisable = [ "all" ];
  configureFlags = [] ++ pkgs.stdenv.lib.optional useGMP [ "--enable-gmp" ];
  preConfigure = ''
    autoconf
    patchShebangs .
  '';
}
