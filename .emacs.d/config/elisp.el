(provide 'elisp)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (paredit-mode +1)
	    (show-paren-mode +1)))
