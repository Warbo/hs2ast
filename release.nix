{
  build = { nixpkgs, hs2ast }:
    with nixpkgs;
    with haskellPackages;
    callPackage hs2ast/default.nix {};
}
