(provide 'scala)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :capf)
  :hook (scala-mode . lsp-deferred))

(use-package lsp-metals :ensure t)
(use-package lsp-ui :ensure t)
(use-package sbt-mode :ensure t)
(use-package scala-mode :ensure t)
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  :hook (scala-mode . yas-minor-mode))
