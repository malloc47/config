(provide 'org-custom)

(defun jw/org-extensions ()
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-level-4 nil :height 1.0))

(use-package org
  :init
  (setq org-return-follows-link t)
  (setq org-startup-with-inline-images t)
  (setq org-directory "~/notes")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list org-directory))
  ;; (setq org-hide-leading-stars t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  :hook ((org-babel-after-execute . org-redisplay-inline-images)
	 (org-mode . jw/org-extensions))
  :bind (:map org-mode-map
	 ("C-c C-SPC" . 'org-mark-ring-goto)
	 ("M-h" . nil))) ; This gets in the way of windmove config

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-directory (file-truename org-directory))
  (setq org-roam-completion-everywhere t)
  (setq org-roam-graph-viewer "chromium")
  :config
  (org-roam-db-autosync-mode)
  :bind (:map org-mode-map
	      ("C-c b" . org-roam-buffer-toggle)
	      ("C-c f" . org-roam-node-find)
	      ("C-c i" . org-roam-node-insert)
	      ("C-c c" . org-roam-capture)
	      ("C-c n" . org-id-get-create)
	      ("C-c g" . org-roam-graph)
	      ("M-."   . org-open-at-point)))

(use-package org-roam-ui :ensure t)

(use-package org-superstar
  :ensure t
  :init
  (setq  org-superstar-special-todo-items t)
  :hook (org-mode))
