{
  HaLVM-GMP = import ./default.nix { useGMP = true; };
  HaLVM-integer-simple = import ./default.nix { useGMP = false; };
}
