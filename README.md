# malloc47/config

NixOS configuration repo for an evolving ensemble of machines/VMs
including:

- VMWare Fusion-hosted VM running on an Intel `MacBookPro16,1`
- VMWare Fusion-hosted VM running an M1 `MacBookPro18,1`
- Baremetal install running on an aging `MacBookAir6,1`

## Design

Mechanically, the files under `hosts/` are the OS-level
`configuration.nix` equivalent, and when bootstrapping a new machine
the relevant file is symlinked to
`/etc/nixos/configuration.nix`. These host files each pull in the
relevant `hardware/` file and the central OS configuration used on all
my machines, confusingly called `nixos/configuration.nix`.

Userspace is controlled by home-manager, which is included as a NixOS
module and (re)built alongside the whole OS. `config/home.nix` is the
primary entrypoint for userspace configuration.

The dotfiles in this repo are handled in a mixture of modes, including:

- Raw dotfiles expressed in their native format that are stored in
  normal files and copied by home-manager using
  `home.file.<name>.source`.

- Dotfiles expressed in their native format but stored as a
  (potentially templated) string in `home.nix` using
  `home.file.<name>.text = ...`.

- Dotfiles that are expressed using nix expression syntax that is then
  rendered as native dotfiles completely by home-manager.

All relevant dotfiles that are stored as independent files live
alongside `home.nix` in the `config/` folder.

### Notable Specifics

As a way of more expressively controlling the differences between
hosts, I use a dedicated set of options in the `modules/settings.nix`
file. This module bundles settings that are commonly needed in
multiple places in both the OS configuration and the userspace
configuration. These include:

- `fontSize`: This varies from one machine to another because my VMs
  run on Retina screens whereas my current baremetal machine is not.

- `terminal`: Needed to vary my terminal on specific machines because
  of an alacritty bug
  malloc47/config@e9a81e60bfdeb637589b75248de9256e1dcf18be .

- `vm`: Boolean indicating whether or not the host is a VM.

- `xkbFile`: Because my machines/VM hosts have different keybindings,
  they need different customizations.

On this last point I swap some specific keys, like `[` <-> `(`, `]`
<-> `)`, and `CapsLock` -> `Ctrl`. On the VM host, some of these are
already swapped, so swapping them again inside the VM guest is not
necessary.

These settings make expressing nix configurations easier. For example,
on a VM, certain keybindings are made on the VM host level so I can
omit them from my i3 keybindings, but need to keep them for baremetal
machines. Rather than hardcoding host names or other proxies for being
on baremetal, such configuration becomes easier to read:

    keybindings = mkOptionDefault (
      {
        "${mod}+p" = "exec ${pkgs.dmenu}/bin/dmenu_run";
        "${mod}+o" = "exec ${pkgs.clipmenu}/bin/clipmenu";
        ...
      }
      // optionalAttrs (!config.settings.vm)
      {
        "${mod}+grave" = "workspace 1";
        "${mod}+Shift+Control+L" = "exec i3lock";
      });

## History

The public portion of this repository started in 2010 after separating
out secrets and other non-public elements from a private repo that I
had been using for another several years prior. It started as a grab
bag of dotfiles that I shared among several Linux machines, including
Fedora, Ubuntu, and my personal Arch machines. These dotfiles were
initially managed by a rudimentary bash script that symlinked them all
into a target home folder, later adding system-specific overrides to
handle multiple hosts.

In 2019, this repo transitioned to holding NixOS + home-manager
configuration, documented in [this
post](https://www.malloc47.com/migrating-to-nixos/). This is a
significant expansion of scope, as most physical hardware/services
were previously left out of this repository.
