{
  description = "Conway's Game of Life in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
        execName = "gol";
        hp = pkgs.haskellPackages; # pkgs.haskell.packages.ghc921;
        project = returnShellEnv:
          hp.developPackage {
            inherit returnShellEnv;
            name = "conways-game-of-life";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7
              # Example: 
              # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
              # Assumes that you have the 'NanoID' flake input defined.
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with hp; [
                # Specify your build/dev dependencies here. 
                ghc
                cabal-install
                hlint
                haskell-language-server
                ormolu

                pkgs.mesa
                pkgs.mesa_glu
                pkgs.freeglut
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        defaultApp = {
          type = "app";
          program = "${self.defaultPackage.${system}}/bin/${execName}";
        };

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
