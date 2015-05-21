{
  build = { nixpkgs, hs2ast }:
    with nixpkgs;
    with nixpkgs.haskellPackages;
    callPackage hs2ast/default.nix {};
}
