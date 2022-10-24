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

        buildStackProject = pkgs.haskell.lib.buildStackProject;

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          version = pkgs.stack.version;
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

        raw-deps = [
          stack-wrapped
        ];

        pkgs-deps = with pkgs; [
          sqlite
          sqlitebrowser
          stdenv.cc.cc.lib
          zlib
        ];

        hs-deps = with haskell-pkgs; [
          ghc
          hlint
          hoogle
          haskell-language-server
          hls-tactics-plugin
          implicit-hie
        ];

        devTools = raw-deps ++ pkgs-deps ++ hs-deps;

      in {
        packages.default = pkgs.callPackage buildStackProject {
          name = "aywins";
          version = "0.0.1";
          src = ./.;

          ghc = haskell-pkgs.ghc;
          stack = stack-wrapped;
          buildInputs = pkgs-deps;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = devTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
        };
      });
}
