let
  aida = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHIJ8w4XJ5Q2T+E0Mr5kvH39LS4INn5MfxaN2eRdskTi root@aida";
  attila = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOPlUC4rL847EA33C4cxd9xJl2BFbh5CMgb72ZLRKpF5 root@attila";
  cesare = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEgj2i1LZ39cDblcHZesMEa0HW03F8UqfWMPRucxPiBb";
  aroldo = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILo3pfY8U0RfVKsDDR3Fqqr/8LthqdTcdS8mmMeeKTjZ root@aroldo";
  malloc47 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6Ql5G8d09APA4ABhzMk0gQtr8CGxeynRGlhOB+1pjk malloc47-2026-03";
in
{
  "nix-access-tokens.age".publicKeys = [
    cesare
    malloc47
  ];
  "caddy-basicauth.age".publicKeys = [
    aida
    malloc47
  ];
  "cloudflare-acme.age".publicKeys = [
    aida
    aroldo
    malloc47
  ];
  "authelia-jwt-secret.age".publicKeys = [
    aida
    malloc47
  ];
  "authelia-storage-key.age".publicKeys = [
    aida
    malloc47
  ];
  "authelia-session-secret.age".publicKeys = [
    aida
    malloc47
  ];
  "authelia-users.age".publicKeys = [
    aida
    malloc47
  ];
  "cloudflared-credentials.age".publicKeys = [
    aida
    aroldo
    malloc47
  ];
  "ntfy-admin-password.age".publicKeys = [
    aroldo
    malloc47
  ];
  "ntfy-admin-password-env.age".publicKeys = [
    aroldo
    malloc47
  ];
  "homepage-env.age".publicKeys = [
    aida
    malloc47
  ];
  "wifi-unimatrix47.age".publicKeys = [
    malloc47
    attila
  ];
}
