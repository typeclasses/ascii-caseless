{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  hls =
    pkgs.haskell-language-server.override { supportedGhcVersions = [ "98" ]; };

  combineOverrides = old: fold composeExtensions (old.overrides or (_: _: { }));

in rec {

  packages = let
    makeTestConfiguration = { ghcVersion, overrides ? new: old: { } }:
      let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
      in (pkgs.haskell.packages.${ghcVersion}.override (old: {
        overrides = combineOverrides old [
          (packageSourceOverrides { ascii-caseless = ../ascii-caseless; })
          overrides
        ];
      })).ascii-caseless;
  in rec {
    ghc-9-2 = makeTestConfiguration { ghcVersion = "ghc92"; };
    ghc-9-4 = makeTestConfiguration { ghcVersion = "ghc94"; };
    ghc-9-6 = makeTestConfiguration {
      ghcVersion = "ghc96";
      overrides = new: old: {
        ascii-char = new.callPackage ./haskell/ascii-char.nix { };
        ascii-case = new.callPackage ./haskell/ascii-case.nix { };
      };
    };
    ghc-9-8 = makeTestConfiguration {
      ghcVersion = "ghc98";
      overrides = new: old: {
        ascii-char = new.callPackage ./haskell/ascii-char.nix { };
        ascii-case = new.callPackage ./haskell/ascii-case.nix { };
      };
    };
    all = pkgs.symlinkJoin {
      name = "ascii-caseless-tests";
      paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ghc-9-8 ];
    };
  };

  devShells.default = pkgs.mkShell {
    inputsFrom = [ packages.ghc-9-8.env ];
    buildInputs = [ pkgs.cabal-install ];
  };

}
