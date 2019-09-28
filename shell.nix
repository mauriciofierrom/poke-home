{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.shellFor {
  packages = ps: [ (import ./release.nix).pokehome ];
  buildInputs = [ pkgs.haskellPackages.ghcid ];
}
