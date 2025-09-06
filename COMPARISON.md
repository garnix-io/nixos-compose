# Comparison with other tools

## `nix build .\#nixosConfigurations.$SERVER.config.system.build.vm && ./result/bin/run-ai-vm`

- Has a long list of ui quirks and gotchas:
  - Opens a graphical window to show textual tty output.
  - You can switch by setting `virtualisation.vmVariant.virtualisation.graphics = false;`, but then it still messes up your terminal.
  - Creates a `.qcow2` file on the first run that will be re-used on subsequent runs.
    Meaning you can change your NixOS config declaration and silently just not get any of your changes.
  - You have to configure a way to log in, either with `virtualisation.vmVariant.services.getty.autologinUser = "root";` or some other manual way.
  - Networking (e.g. forwarding ports to ssh in, setting up networking between VMs) is all up to manual configuration.
- Is not very discoverable.
- Seems more like a low-level building block than a thought out UI.

## microvm.nix

- Solves some of the problems with running the vm scripts. (E.g. doesn't mess up your terminal.)
- Requires more manual configuration and work for e.g. ssh access and networking
  between VMs. Both in terms of nix config and running commands on the host.
- Seems to be mostly targeted at running production systems in VMs on NixOS machines.

## nixos-container

- Allows to spin up containers (not full VMs) from NixOS configs.
- Seems to only work on NixOS hosts.
- Shares the network with the host (could lead to port clashes), or requires manual configuration.
- Logging into containers requires manually setting up ssh.

## `nixos-rebuild build-vm`

- Provides a slightly nicer way to run `nix build .\#nixosConfigurations.$SERVER.config.system.build.vm`.
- Is meant to manage non-VM NixOS systems, i.e. `nixos-rebuild switch`.
- Building and running VMs for testing is an additional feature.

## `nixos-build-vms`

- Doesn't work with flake files.
- Seems a bit bitrotten? Couldn't figure out how to make it work.
- It's a ~50 line bash script around `nix-build`.
