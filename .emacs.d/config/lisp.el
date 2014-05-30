(provide 'lisp)

(defun enable-show-paren-mode ()
  (show-paren-mode +1)
  (setq show-paren-delay 0
	show-paren-style 'parenthesis))
