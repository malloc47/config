{ ... }:
{
  home.file."setup-shared" = {
    target = "bin/setup-shared";
    executable = true;
    text = ''
      #!/usr/bin/env bash
      mkdir $HOME/shared 2>/dev/null
      vmhgfs-fuse .host:/shared $HOME/shared -o subtype=vmhgfs-fuse
    '';
  };
}
