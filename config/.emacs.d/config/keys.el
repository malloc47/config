(provide 'keys)

(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)

;; Disable M-h in org-mode in favor of windmove-left
(define-key org-mode-map (kbd "M-h") nil)

(global-set-key (kbd "C-x C-S-f") 'find-file-in-project)

(global-set-key (kbd "C-x C-g") 'magit-status)

(global-set-key (kbd "C-c C-g t") (function
				   (lambda ()
				     "Go to top of page."
				     (interactive)
				     (move-to-window-line 0))))

(global-set-key (kbd "C-c C-g b") (function
				   (lambda ()
				     "Go to bottom of page."
				     (interactive)
				     (move-to-window-line -1))))

(global-set-key (kbd "C-c C-g m") (function
				   (lambda ()
				     "Go to middle of page."
				     (interactive)
				     (move-to-window-line (/ (window-height) 2)))))
