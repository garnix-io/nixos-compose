{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import "${nixpkgs}" { inherit system; }; in
      rec {
        packages = {
          default = pkgs.haskellPackages.callCabal2nix "cradle" ./. { };
        };
        checks = {
          hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; }
            ''
              cd ${./.}
              hlint src test
              touch $out
            '';
        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              ghcid
              ormolu
              cabal-install
              (pkgs.haskellPackages.ghc.withPackages (p: packages.default.buildInputs))
              packages.default.buildInputs
              (haskell-language-server.override { dynamic = true; })
              pkgs.haskellPackages.cabal2nix
              nixpkgs-fmt
              nil
              hlint
            ];
          };
        };
      }
    );
}

