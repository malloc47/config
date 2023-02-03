(provide 'elisp)

(use-package elisp-mode
  :no-require t
  :hook ((emacs-lisp-mode . paredit-mode)
	 (emacs-lisp-mode . prettify-symbols-mode)
	 (emacs-lisp-mode . show-paren-mode)))
