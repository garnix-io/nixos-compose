# `nixos-compose`

`nixos-compose` makes it easy to spin up vms locally based on nixos configurations from flake files.

## simple example

In a directory with a flake file that contains a nixos configuration `server`, you can do:

``` bash
$ nix flake show
git+file:///home/shahn/garnix/nixos-compose
└───nixosConfigurations
    └───server: NixOS configuration
$ nixos-compose up server
# wait for vm to boot up
$ nixos-compose ssh server
[vmuser@server:~]$ journalctl
[vmuser@server:~]$ exit
$ nixos-compose down
```

## installation

`nixos-compose` is available through the flake at `github:garnix-io/nixos-compose`.

## features

### ssh access

`nixos-compose` injects some configuration into the nixos configs to enable ssh and add a `vmuser`.
This allows you to ssh into vms without any manual configuration.

### cross-vm networking

`nixos-compose` creates a virtual network switch and connects the vms to it.
It also injects entries into the `/etc/hosts` file in the vms so that they can access each other by the names of the nixos configurations in the flake file.
So for example, if you spin up two machines `a` and `b`, they can talk to each other by the domain names `a` and `b`.
Without any manual configuration on your part.

### tap

`nixos-compose tap` sets up a virtual network device on the host system that allows you to connect to the vms through their ip addresses.
(`nixos-compose ip $SERVER` will show you the vm's ip address.)


## `nixos-compose --help`

```
Usage: nixos-compose COMMAND

Available options:
  -h,--help                Show this help text
  --version                Show version (405f14a) and exit

Available commands:
  up                       Start development vms
  down                     Stop running vms
  ssh                      `ssh` into a running vm
  status                   Show the status of running vms
  list                     List all configured vms
  ip                       Print the ip address of a vm (in the virtual network)
  tap                      Set up a tap device, to allow network access to vms
                           from the host (uses `sudo`)
```

## feedback

Feedback (bug reports, feature requests, etc.) very much appreciated at [https://github.com/garnix-io/nixos-compose/issues](https://github.com/garnix-io/nixos-compose/issues).
