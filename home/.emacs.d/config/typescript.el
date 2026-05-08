(provide 'typescript)

(use-package tide
  :ensure t
  :hook ((typescript-mode . tide-setup)
	 (before-save . tide-format-before-save)))

(defun jw/typescript-extensions ()
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(use-package flycheck
  :hook typescript-mode)

(use-package typescript-mode
  :hook (typescript-mode . jw/typescript-extensions))
