{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
 thisPackage = pkgs.haskellPackages.callPackage ./default.nix {};
in
pkgs.haskellPackages.shellFor {
  buildInputs = [
    cabal2nix
    cabal-install
    haskell-language-server
  ];
  withHoogle = true;
  packages = p: [ thisPackage ];
}
