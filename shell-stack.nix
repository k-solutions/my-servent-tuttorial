{nixpkgs ? import <nixpkgs> { }, ghcCompiler ? "ghc8107"}:

let
  inherit (nixpkgs) pkgs;
  glbPkgs = with pkgs; [
     zlib.dev
     zlib.out
     # sqlite
   ]; 
   ghc = pkgs.haskell.packages.${ghcCompiler}.ghcWithPackages (ps: with ps; [
      haskell-language-server
      hlint
      stylish-haskell
      # haddock
      hoogle
      aeson
      servant-server
      wai
      warp
      postgresql-simple
      resource-pool
  ]);  
in pkgs.haskell.lib.buildStackProject {
  name = "my-servant-tut";
  buildInputs = glbPkgs ++ [ 
    ghc
  ];
  inherit ghc;
}
