# malloc47/config

Nix/NixOS/nix-darwin configuration repo for an evolving ensemble of
machines/VMs including:

- LXC Container running on Ubuntu 22.04 on a Thinkpad X1 Carbon Gen 9
- nix-darwin on a recent Macbook Pro Mac16,6
- VMWare Fusion-hosted VM running on this ^ same Macbook Pro Mac16,6
- Baremetal install running on an aging `MacBookAir6,1`
- VMWare Fusion-hosted VM running on an aging `MacBookPro11,3`

> [!NOTE]
> This config is undergoing an overhaul and has only partially adopted
> flakes.

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

### Containerization

I run NixOS natively on my own hardware but, on my work hardware, I
find it more pragmatic to run containerized / virtualized for several
reasons:

1. It takes time to troubleshoot minor hardware interactions
   (brightness controls, volume controls, suspend, fingerprint reader,
   etc.) that are already setup on the host OS.

2. Work-mandated software (endpoint protection, VPNs) generally is not
   well-supported (or even allowed) on non-vendor-supplied OSes.

3. IT support tends to be nonexistent for highly-custom OS installs,
   so routine breakages or glitches would be entirely on me to debug.

It's conceivably possible to configure the host OS to work similarly
to how I've configured NixOS, but the overhead of optimizing for
several decades of trained muscle memory that is my keybindings
coupled with the lack of reproducibility I've come to rely on with
NixOS make the overhead of dealing with a VM preferable to me.

On Macs, VMWare Fusion has been my VM host of choice. For efficiency,
I often run the browser natively in OSX on a full-screen workspace and
run the VM full-screen on another workspace, with customized
keybindings to make switching between them match my "native"
keybindings (e.g., I bind ``Super-` `` to "switch to browser
workspace" in OSX which is next to `Super-1` as the first i3 workspace
in the VM).

On work-provided Linux machines, the story is more complicated. For
simplicity, I could mimic OSX and virtualize NixOS on my Linux host
also. However, the option to reduce resource usage (VMs do eat battery
life) on my Linux host by running NixOS as a lightweight container is
too tempting to pass up. I have specifically set this up on an Ubuntu
22.04 host using [LXC](https://linuxcontainers.org/) as the container
runtime. A Docker-based approach could likely be made to work too, and
is something I should explore at some point, but LXC is more natural
for running a full, persistent OS vs a single application and is a
[supported
build](https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/virtualisation/lxc-container.nix)
in NixOS.

A nontrivial chunk of my setup relies on having graphical support (an
X server) available--everything from my tiling window manager, to my
emacs running in GUI mode. One option that I did ultimately get
working was to have the container itself host an X server side-by-side
with the Ubuntu X server, with the container X server running my i3
window manager. While this worked, it was awkward to switch between
the individual X sessions (particularly on a laptop where I was
occasionally toggling from docked to mobile with associated display
changes) which I often needed to do to manage VPNs or other related
work tools that I did not want running in my container.

So, instead, I ultimately decided to run only a single X server on the
host and have the container connect to this X server to launch
graphical applications. Like OSX, Gnome isn't sufficiently
customizable to make it match closely enough to work as I want, but
thankfully Ubuntu has no trouble running i3 as an alternative window
manager. This, however, creates a problem: My i3 configuration in
NixOS is generated via home-manager which would be running inside the
container but running i3 on the host requires this configuration be
made available outside the container. By consistently mounting the
container's root filesystem (I use `~/lxc-share`), this can be solved
by symlinking the config (run from the host):

    ln -s ~/lxc-share/.config/i3/config ~/.config/i3/config

Not exactly elegant, but it does allow me to keep my i3 config
controlled by NixOS. Another issue, though, is that my i3 config
references keybindings to launch specific applications that exist
within the container. By having i3+config live and run outside the
container, I had to further specialize the i3 config to wrap calls
with `lxc exec` so that the proper containerized applications are
executed. This is further complicated because you do not get a "full"
shell with, say, `lxc exec nixos -- /run/current-system/sw/bin/zsh`;
you'll be missing dbus, `XDG_*` variables, etc.. So an additional
`machinectl shell` wrapper is needed inside of `lxc exec` for
applications to have an environment that matches what they would have
if a full-blown X session was running in the container.

To make working with both the host and guest as seamless as possible,
I set up some "dual" keybindings to create a unified experience:

- `Super+Return`: Container terminal
- `Super+Shift+Return`: Host terminal
- `Super+p` : Container dmenu runner
- `Super+Shift+P` : Host dmenu runner

This allows me to launch specific applications on the host
(`Super+Shift+P vpn`) and others on the container (`Super+p emacs`)
that all appear seamlessly within the same window manager. If I need
to do any mainainance operations (like fetching the latest approved
software updates), I can launch a terminal on the host to run the
appropriate commands. For most everything else I do on my work
machine, I launch terminals or applications in the container.

Unlike Docker, LXC doesn't have a `Dockerfile` equivalent. To
facilitate setting up the container, this repo includes an
[lxc.sh](lxc/lxc.sh) script that resets LXC to a clean state and
(re)builds/installs the NixOS container image, setting up all the
necessary hardware permissions needed to allow container applications
access to the X server, audio, and network, as well as
container-specific settings such as the storage mounts.

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
