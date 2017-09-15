{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./cryfsm.nix {}
