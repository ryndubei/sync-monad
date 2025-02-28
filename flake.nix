{
  description = "A monad for communication protocols";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-2411";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, haskellNix, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend (haskellNix.overlay);
        project = pkgs.haskell-nix.cabalProject' {
          name = "sync-monad";
          src = pkgs.haskell-nix.cleanSourceHaskell {
            src = ./.;
            name = "sync-monad-src";
          };
          compiler-nix-name = "ghc8107";
          shell.tools = {
            cabal = { };
            haskell-language-server = {
              src = pkgs.haskell-nix.sources."hls-2.2";
            };
          };
          shell.buildInputs = [
            (pkgs.writeScriptBin "haskell-language-server-wrapper" ''
              #!${pkgs.stdenv.shell}
              exec haskell-language-server "$@"
            '')
          ];
        };
      in project.flake { });

  nixConfig.extra-substituters =
    [ "https://cache.iog.io" "https://cache.zw3rk.com" ];
}
