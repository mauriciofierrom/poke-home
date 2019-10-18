{ pkgs ? import <nixpkgs> {} }:
with pkgs; {
  shell = haskellPackages.shellFor {
    packages = ps: [ (import ./release.nix).pokehome ];
    buildInputs = [ haskellPackages.ghcid ];
  };
}.shell
