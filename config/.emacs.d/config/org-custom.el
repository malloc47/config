(provide 'org-custom)

(require 'subr-x)

(defun jw/org-extensions ()
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-level-4 nil :height 1.0))

(use-package org
  :init
  (setq org-startup-with-inline-images t)
  (setq org-directory "~/notes")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list org-directory))
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-roam-node-display-template
	(concat "${title:*} "
		(propertize "${tags:50}" 'face 'org-tag)))
  (require 'ox-md)
  ;; (setq org-hide-leading-stars t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  :hook ((org-babel-after-execute . org-redisplay-inline-images)
	 (org-mode . jw/org-extensions)
	 (org-mode . turn-on-flyspell))
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
  (setq org-roam-capture-templates
	`(("d" "default" plain "%?"
	   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n")
	   :unnarrowed t)
	  ("t" "technology" entry "* ${title} \n%?"
	   :target (file+head  "technology/${slug}.org"
			       ,(string-join
				 '(":PROPERTIES:"
				   ":ID: ${slug}"
				   ":END:"
				   "#+TITLE: ${title}"
				   "#+FILETAGS: :technology:")
				 "\n"))
	   :unnarrowed t)
	  ("e" "team" entry "* ${title} \n%?"
	   :target (file+head  "teams/${slug}.org"
			       ,(string-join
				 '(":PROPERTIES:"
				   ":ID: ${slug}"
				   ":END:"
				   "#+TITLE: ${title}"
				   "#+FILETAGS: :team:")
				 "\n"))
	   :unnarrowed t)))
  (defun org-roam-custom-help ()
    (interactive)
    (message "[b]uffer, [f]ind, [i]nsert, [c]apture, [n]ew node, [g]raph"))
  :config
  (org-roam-db-autosync-mode)
  :bind (:map org-mode-map
	      ("C-c b" . org-roam-buffer-toggle)
	      ("C-c f" . org-roam-node-find)
	      ("C-c i" . org-roam-node-insert)
	      ("C-c c" . org-roam-capture)
	      ("C-c n" . org-id-get-create)
	      ("C-c g" . org-roam-graph)
	      ("C-c h" . org-roam-custom-help)
	      ("M-."   . org-open-at-point)))

(use-package org-roam-ui :ensure t)

(use-package org-superstar
  :ensure t
  :init
  (setq  org-superstar-special-todo-items t)
  :hook (org-mode))
