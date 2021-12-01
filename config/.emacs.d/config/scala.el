(provide 'scala)

(ensure-packages-installed 'lsp-mode 'lsp-metals 'lsp-ui 'scala-mode 'sbt-mode 'yasnippet)

(require 'lsp-mode)
(require 'lsp-metals)
(require 'yasnippet)

(setq lsp-keymap-prefix "C-c C-l")

(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil)
(setq lsp-completion-provider :capf)

(yas-reload-all)
(add-hook 'scala-mode-hook #'yas-minor-mode)

(add-hook 'scala-mode-hook #'lsp-deferred)
