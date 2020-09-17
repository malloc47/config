(provide 'scala)

(ensure-packages-installed 'lsp-mode 'lsp-ui 'company 'company-lsp
			   'scala-mode 'sbt-mode 'yasnippet)

(require 'lsp-mode)
(require 'company-lsp)
(require 'yasnippet)

(setq lsp-keymap-prefix "C-c C-l")

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)

(yas-reload-all)
(add-hook 'scala-mode-hook #'yas-minor-mode)

(add-hook 'scala-mode-hook #'lsp-deferred)
