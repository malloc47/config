(provide 'scala)

(ensure-packages-installed 'lsp-mode 'lsp-ui 'company-lsp 'scala-mode 'sbt-mode)

(setq lsp-keymap-prefix "C-c C-l")

(require 'lsp-mode)

(add-hook 'scala-mode-hook #'lsp-deferred)
