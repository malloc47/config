(provide 'elisp)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (paredit-mode +1)
;	    (prettify-symbols-mode +1)
	    (show-paren-mode +1)))
