(provide 'keys)

(use-package windmove
  :bind (("M-h" . 'windmove-left)
	 ("M-l" . 'windmove-right)
	 ("M-k" . 'windmove-up)
	 ("M-j" . 'windmove-down)))

;; https://emacs.stackexchange.com/a/53430
(eval-after-load "nxml-mode"
  (lambda ()
    (define-key nxml-mode-map "\M-h" nil)))
