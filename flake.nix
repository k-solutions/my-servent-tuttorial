{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      hlsPlan = "/nix/store/jxhz8kgb3f854midd2wgda9hp0i59sma-haskell-language-server-1.5.1.0";
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          helloProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              checkMaterialization = true;
              # materialized = ./my-servant-tut.materialized;
              name = "my-servant-tut";
              # index-state = "2021-12-26T00:00:00Z";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {
                  # materilized = if __pathExists hlsPlan then hlsPlan else null;
                };
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                haskellPackages.implicit-hie
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatform = p: [p.ghcjs];
            };
        })
      ];
    
      pkgs = import nixpkgs { 
        inherit system overlays; 
        inherit (haskellNix) config; 
      };

      flake = pkgs.helloProject.flake {
        # This adds support for `nix build .#js-unknown-ghcjs-cabal:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."my-servant-tut:exe";
    });
}
