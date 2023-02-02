(provide 'org-custom)

(ensure-packages-installed 'org-roam 'org-roam-ui)

(require 'org)
(require 'org-roam)

(add-hook 'org-mode-hook
	  (lambda ()
	    (fido-mode)
	    ;; This gets in the way of windmove config
	    (define-key org-mode-map (kbd "M-h") nil)
	    ;; Mirror C-u C-SPC when popping the buffer mark
	    (define-key org-mode-map (kbd "C-c C-SPC") 'org-mark-ring-goto)
	    (local-set-key (kbd "C-c b") 'org-roam-buffer-toggle)
	    (local-set-key (kbd "C-c f") 'org-roam-node-find)
	    (local-set-key (kbd "C-c i") 'org-roam-node-insert)
	    (local-set-key (kbd "C-c c") 'org-roam-capture)
	    (local-set-key (kbd "C-c n") 'org-id-get-create)
	    (local-set-key (kbd "C-c g") 'org-roam-graph)
	    (local-set-key (kbd "M-.")   'org-open-at-point)))

(setq org-return-follows-link t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

;; Display images inline when loading file AND after evaluating babel
(setq org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-roam-directory (file-truename "~/notes"))
(setq org-roam-completion-everywhere t)
(setq org-roam-graph-viewer "chromium")
(org-roam-db-autosync-mode)
