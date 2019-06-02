let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          pokehome =
            haskellPackagesNew.callPackage ./poke-home.nix { };

            pokeapi =
              haskellPackagesNew.callPackage ./pokeapi.nix { };

            dialog-flow =
              haskellPackagesNew.callPackage ./dialog-flow.nix { };
            };
          };
        };
      };

  pkgs = import <nixpkgs> { inherit config; };

in
  { pokehome = pkgs.haskellPackages.pokehome; }
