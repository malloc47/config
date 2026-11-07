let
  attila = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOPlUC4rL847EA33C4cxd9xJl2BFbh5CMgb72ZLRKpF5 root@attila";
  cesare = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEgj2i1LZ39cDblcHZesMEa0HW03F8UqfWMPRucxPiBb";
  malloc47 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6Ql5G8d09APA4ABhzMk0gQtr8CGxeynRGlhOB+1pjk malloc47-2026-03";
  malloc47-user = "age19a3dcu3xrl77my69a38uw62ret4edv2x87rkxedgndlpyacfsdeqkd0h2h";
in
{
  "nix-access-tokens.age".publicKeys = [
    cesare
    malloc47
  ];
  "wifi-unimatrix47.age".publicKeys = [
    malloc47
    attila
  ];
  "gh-token-github.age".publicKeys = [
    malloc47
    malloc47-user
    attila
    cesare
  ];

  "deepseek-api-key.age".publicKeys = [
    malloc47
    malloc47-user
    attila
    cesare
  ];
}
