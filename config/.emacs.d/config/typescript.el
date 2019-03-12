(provide 'typescript)

(ensure-packages-installed 'tide)

(add-hook 'typescript-mode-hook
          (lambda ()
	    (interactive)
	    (tide-setup)
	    (flycheck-mode +1)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode +1)
	    (tide-hl-identifier-mode +1)))

(add-hook 'before-save-hook 'tide-format-before-save)
