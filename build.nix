{ pkgs ? import <nixpkgs> {} }:

{
  app = pkgs.haskellPackages.callPackage ./default.nix {};
}
