{
  description = "Are ya winning son? - A Discord bot for tracking game scores";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        haskell-pkgs = pkgs.haskell.packages.ghc902;

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };

        devTools = [
          stack-wrapped
        ] ++

        (with pkgs; [
          sqlite
        ]) ++

        (with haskell-pkgs; [
          ghc
          hlint
          hoogle
          haskell-language-server
          implicit-hie
        ]);

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = devTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
        };
      });
}
