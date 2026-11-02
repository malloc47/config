(provide 'jw-tramp)

;; tramp-rpc (/rpc:host:/path) is installed by Nix from the same release as
;; pkgs.tramp-rpc-server, so client and server agree on the protocol
;; revision.  Hosts whose home-manager config installs the server use that
;; binary; anywhere else tramp-rpc downloads and deploys its own.
(use-package tramp-rpc
  :after tramp
  :config
  (connection-local-set-profile-variables
   'jw/tramp-rpc-nixos
   '((tramp-rpc-deploy-never-deploy . t)
     (tramp-rpc-deploy-remote-binary-path
      . "/etc/profiles/per-user/malloc47/bin/tramp-rpc-server")))
  (connection-local-set-profile-variables
   'jw/tramp-rpc-home-manager
   '((tramp-rpc-deploy-never-deploy . t)
     (tramp-rpc-deploy-remote-binary-path
      . "~/.nix-profile/bin/tramp-rpc-server")))
  (connection-local-set-profiles
   '(:application tramp :protocol "rpc" :machine "attila")
   'jw/tramp-rpc-nixos)
  ;; agent is the ssh alias for chvld-jwaggone1
  (dolist (machine '("agent" "chvld-jwaggone1"))
    (connection-local-set-profiles
     `(:application tramp :protocol "rpc" :machine ,machine)
     'jw/tramp-rpc-home-manager)))
