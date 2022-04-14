{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides =
            final.lib.composeExtensions prev.haskell.packageOverrides
            (hsFinal: hsPrev: {
              control-io-state =
                hsFinal.callCabal2nix "control-io-state" ./. { };
            });
        };
      };
    in flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
        hs = pkgs.haskellPackages;
      in rec {
        packages = rec {
          control-io-state = hs.control-io-state;
          default = control-io-state;
        };

        devShells = {
          default = hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ control-io-state ];
            nativeBuildInputs = with hs; [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
            ];
          };
          ci = hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ control-io-state ];
            nativeBuildInputs = with hs; [ cabal-install fourmolu pkgs.nixfmt ];
          };
        };
      }) // {
        overlays.default = overlay;
      };
}
