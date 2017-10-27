(provide 'osx)

(ensure-packages-installed 'exec-path-from-shell)

(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (exec-path-from-shell-initialize))
