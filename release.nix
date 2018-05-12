{ pkgs ? import <nixpkgs> {} }:
{
  default = pkgs.haskellPackages.callPackage ./default.nix { };
}
