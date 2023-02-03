(provide 'osx)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :init
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  :config
  (exec-path-from-shell-initialize))
