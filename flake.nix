# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "conways-game-of-life";

        execName = "gol";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        defaultPackage = self.packages.${system}.${packageName};

        apps.${execName} = {
          type = "app";
          program = "${self.packages.${system}.${packageName}}/bin/${execName}";
        };

        defaultApp = self.apps.${system}.${execName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghc
            cabal-install
            hlint
            pkgs.haskell-language-server
            pkgs.ormolu

            pkgs.mesa
            pkgs.mesa_glu
            pkgs.freeglut
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
