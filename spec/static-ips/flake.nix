{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/3ff0e34b1383648053bba8ed03f201d3466f90c9";
  outputs = { nixpkgs, ... }:
    let
      common = {
        nixpkgs.hostPlatform = "x86_64-linux";
        system.stateVersion = "25.05";
      };
    in
    {
      nixosConfigurations.a = (nixpkgs.lib.nixosSystem {
        modules = [
          { networking.hostName = "a"; }
          common
        ];
      });
      nixosConfigurations.b = (nixpkgs.lib.nixosSystem {
        modules = [
          { networking.hostName = "b"; }
          common
        ];
      });
    };
}
