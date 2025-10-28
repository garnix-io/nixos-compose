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
  inputs.garnix-lib = {
    url = "github:garnix-io/garnix-lib";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, flake-utils, cradle, garnix-lib }:
    (flake-utils.lib.eachDefaultSystem (system:
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
              ./nixos-compose.cabal
              ./src
            ];
          };
        haskellPackage = pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "nixos-compose" prodSrc { }) (old: {
          buildDepends = (old.buildDepends or [ ]) ++ [ pkgs.openssh pkgs.nix ];
          doCheck = false;
          configureFlags = [
            "--ghc-option=-Werror"
            "--ghc-option=-O2"
            "--ghc-option=-D__NIXOS_COMPOSE_VERSION__=${self.shortRev or self.dirtyShortRev}"
          ];
        });
        runtimeDeps = [
          (if pkgs.stdenv.isLinux then pkgs.iproute2 else pkgs.iproute2mac)
          pkgs.nix
          pkgs.openssh
          pkgs.vde2
          pkgs.which
        ];
        testDeps = [
          pkgs.bash
          pkgs.coreutils
          pkgs.ps
          pkgs.python3
          pkgs.tree
        ];
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
            ./nixos-compose.cabal
            ./src
            ./spec
            ./.golden
          ];
        };
      in
      rec {
        packages = {
          default =
            let
              mkShellCompletion = shell: outPath: ''
                mkdir -p $out/${builtins.dirOf outPath}
                ${haskellPackage}/bin/nixos-compose --${shell}-completion-script ${haskellPackage}/bin/nixos-compose > $out/${outPath}
              '';
            in
            pkgs.runCommand haskellPackage.name
              {
                nativeBuildInputs = [ pkgs.makeWrapper ];
              }
              ''
                mkdir -p $out/bin/
                cp -r ${haskellPackage}/bin/. $out/bin/
                wrapProgram "$out/bin/nixos-compose" \
                  --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}

                ${mkShellCompletion "bash" "share/bash-completion/completions/nixos-compose.bash"}
                ${mkShellCompletion "zsh" "share/zsh/vendor-completions/_nixos-compose"}
                ${mkShellCompletion "fish" "share/fish/vendor_completions.d/nixos-compose.fish"}
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
        apps = {
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
                  rm dist-newstyle -rf
                  hpack
                  ghcid \
                    --command "cabal repl test:spec --ghc-options=-fdefer-typed-holes" \
                    --allow-eval \
                    --test ":main --skip Integration $*" \
                    --warnings \
                '';
              });
            };

        };
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; ([
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
            ] ++
            runtimeDeps ++
            testDeps);
          };
        };
      }
    )) //
    {
      nixosConfigurations."test-vm" = nixpkgs.lib.nixosSystem {
        modules = [
          garnix-lib.nixosModules.garnix
          ({ pkgs, ... }: {
            networking.hostName = "test-vm";
            nixpkgs.hostPlatform = "x86_64-linux";
            system.stateVersion = "25.05";
            garnix.server.enable = true;
            networking.firewall.allowedTCPPorts = [ 80 ];
            services.nginx = {
              enable = true;
              virtualHosts.default.locations."/".return = "200 'hello from test-vm\\n'";
            };
          })
        ];
      };
    };
}
