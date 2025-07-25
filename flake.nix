{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.cradle = {
    url = "github:garnix-io/cradle";
    inputs = {
      nixpkgs.follows = "nixpkgs";
      flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, cradle }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" { inherit system; config.allowBroken = true; };
        lib = pkgs.lib;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            cradle = cradle.lib.${system}.mkCradle final;
          };
        };
        prodSrc =
          lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./vmcli.cabal
              ./src
            ];
          };
        haskellPackage = pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "vmcli" prodSrc { }) (old: {
          buildDepends = (old.buildDepends or [ ]) ++ [ pkgs.openssh pkgs.nix ];
          doCheck = false;
          configureFlags = [
            "--ghc-option=-Werror"
            "--ghc-option=-O2"
          ];
        });
        runtimeDeps = [ pkgs.openssh pkgs.nix ];
        ghcWithDeps =
          (
            let
              withTestDeps = pkgs.haskell.lib.overrideCabal haskellPackage (old: { doCheck = true; });
            in
            haskellPackages.ghc.withPackages (p: withTestDeps.buildInputs)
          );
        devSrc = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            ./vmcli.cabal
            ./src
            ./spec
            ./.golden
          ];
        };
      in
      rec {
        packages = {
          # todo: add dependencies to runtime deps
          default =
            let
              mkShellCompletion = shell: outPath: ''
                mkdir -p $out/${builtins.dirOf outPath}
                ${haskellPackage}/bin/vmcli --${shell}-completion-script ${haskellPackage}/bin/vmcli > $out/${outPath}
              '';
            in
            pkgs.runCommand haskellPackage.name
              {
                nativeBuildInputs = [ pkgs.makeWrapper ];
              }
              ''
                mkdir -p $out/bin/
                cp -r ${haskellPackage}/bin/. $out/bin/
                wrapProgram "$out/bin/vmcli" \
                  --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}

                ${mkShellCompletion "bash" "share/bash-completion/completions/vmcli.bash"}
                ${mkShellCompletion "zsh" "share/zsh/vendor-completions/_vmcli"}
                ${mkShellCompletion "fish" "share/fish/vendor_completions.d/vmcli.fish"}
              '';
        };
        checks = {
          hlint = pkgs.runCommand "hlint" { buildInputs = [ pkgs.hlint ]; }
            ''
              cd ${devSrc}
              hlint src spec -XQuasiQuotes
              touch $out
            '';
        };
        apps =
          let testDeps = [ pkgs.coreutils pkgs.bash ];
          in {
            spec = {
              type = "app";
              program = pkgs.lib.getExe (pkgs.writeShellApplication {
                name = "spec";
                inheritPath = false;
                runtimeInputs = [
                  ghcWithDeps
                  pkgs.cabal-install
                ] ++
                runtimeDeps ++
                testDeps;
                text = ''
                  dir=$(mktemp -d)
                  trap 'rm -r $dir' EXIT
                  cd "$dir"
                  cp -r ${devSrc}/. .
                  chmod -R a+w .
                  cabal run spec --ghc-option=-Werror -- --strict
                '';
              });
            };
            watch =
              {
                type = "app";
                program = pkgs.lib.getExe (pkgs.writeShellApplication {
                  name = "watch";
                  inheritPath = false;
                  runtimeInputs = [
                    ghcWithDeps
                    pkgs.cabal-install
                    pkgs.ghcid
                    pkgs.hpack
                  ] ++
                  runtimeDeps ++
                  testDeps;
                  text = ''
                    hpack
                    ghcid \
                      --command "cabal repl test:spec" \
                      --allow-eval \
                      --test ":main --skip Integration $*" \
                      --warnings \
                  '';
                });
              };

          };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              (haskell-language-server.override { dynamic = true; })
              ghcWithDeps
              cabal-install
              ghcid
              haskellPackages.cabal2nix
              haskellPackages.hspec-discover
              hlint
              nil
              nixpkgs-fmt
              ormolu
              packages.default.buildInputs
            ];
          };
        };
      }
    );
}
