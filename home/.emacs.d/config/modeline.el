(provide 'modeline)

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode 1))
