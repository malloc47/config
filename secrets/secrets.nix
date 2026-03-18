let
  aida = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHIJ8w4XJ5Q2T+E0Mr5kvH39LS4INn5MfxaN2eRdskTi root@aida";
  malloc47 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE6Ql5G8d09APA4ABhzMk0gQtr8CGxeynRGlhOB+1pjk malloc47-2026-03";
in
{
  "caddy-basicauth.age".publicKeys = [
    aida
    malloc47
  ];
}
