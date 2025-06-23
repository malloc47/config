#!/usr/bin/env bash
FILE=$(mktemp -p $(pwd))
plutil -convert json -o - ~/Library/Preferences/com.apple.symbolichotkeys.plist | jq . > $FILE
nix-instantiate --eval -E "builtins.fromJSON (builtins.readFile $FILE)" | nixfmt > keyboard.nix
rm $FILE
