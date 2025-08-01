{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/3ff0e34b1383648053bba8ed03f201d3466f90c9";
  outputs = { self, nixpkgs, ... }:
    let
      common = {
        nixpkgs.hostPlatform = "x86_64-linux";
        system.stateVersion = "25.05";
      };
    in
    {
      nixosConfigurations.server = (nixpkgs.lib.nixosSystem {
        modules = [
          {
            networking.hostName = "server";
            services.nginx = {
              enable = true;
              virtualHosts.default.locations."/".return = "200 'hello from nginx'";
            };
            networking.firewall.allowedTCPPorts = [ 80 ];
          }
          common
        ];
      });
      nixosConfigurations.client = (nixpkgs.lib.nixosSystem {
        modules = [
          ({ pkgs, ... }: {
            networking.hostName = "client";
            environment.systemPackages = [
              (pkgs.writeShellApplication {
                name = "fetch-from-server";
                runtimeInputs = [ pkgs.curl ];
                text = "curl http://server/";
              })
            ];
          })
          common
        ];
      });
    };
}
