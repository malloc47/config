(provide 'org-custom)

(require 'subr-x)

(defun jw/org-extensions ()
  (set-face-attribute 'org-level-2 nil :height 1.0)
  (set-face-attribute 'org-level-3 nil :height 1.0)
  (set-face-attribute 'org-level-4 nil :height 1.0))

(setq jw/notes-directory "~/notes")

(use-package ox-slack
  :ensure t
  :config
  (require 'ox-slack)
  ;; It's pointless to export my internal links to org files, so only
  ;; export contents of links
  (defun jw-org-slack-link-remove-internal (link contents info)
    (when (equal "id" (org-element-property :type link)) contents))
  ;; https://github.com/titaniumbones/ox-slack/pull/9/files
  (defun jw-org-slack-link-fix-output (link-string)
    (if (string-match (rx line-start "*" (group (+ (not "*"))) "* (" (group (+ (not ")"))) ")") link-string)
	(format "[%s](%s)" (match-string 1 link-string) (match-string 2 link-string))
      link-string))
  (advice-add 'org-slack-link :before-until #'jw-org-slack-link-remove-internal)
  (advice-add 'org-slack-link :filter-return #'jw-org-slack-link-fix-output))

(use-package org
  :init
  (setq org-startup-with-inline-images t)
  (when (file-directory-p jw/notes-directory)
    (setq org-directory jw/notes-directory)
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (setq org-agenda-files
	  (mapcar (lambda (folder) (concat (expand-file-name org-directory) "/" folder))
		  (delete "directory" (directory-files org-directory nil "^[[:alnum:]]+$")))))
  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-export-with-section-numbers nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-roam-node-display-template
	(concat "${title:*} "
		(propertize "${tags:50}" 'face 'org-tag)))
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation nil)
  (require 'ox-md)
  (require 'org-tempo)
  (tempo-define-template
   "org-graphviz"
   '("#+BEGIN_SRC dot :file graph.svg :cmdline -Kdot -Tsvg" n
     "digraph {" n
     "  bgcolor=\"transparent\";" n
     "  rankdir=LR;" n
     "  splines=true;" n
     "  " p n
     "}" n
     "#+END_SRC")
   "<dot"
   "Inline graphviz template")
  ;; (setq org-hide-leading-stars t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (shell . t)
     (mermaid . t)))
  :hook ((org-babel-after-execute . org-redisplay-inline-images)
	 (org-mode . jw/org-extensions)
	 (org-mode . turn-on-flyspell)
	 (org-mode . auto-fill-mode))
  :bind (:map org-mode-map
	      ("C-c C-SPC" . 'org-mark-ring-goto)
	      ("M-h" . nil)  ; This gets in the way of windmove config
	      ("C-u" . universal-argument)))

(use-package org-roam
  :ensure t
  :after org
  :init
  (when (file-directory-p jw/notes-directory)
    (setq org-roam-directory (file-truename org-directory))
    (setq org-roam-file-exclude-regexp
	  ;; org-agenda ignores the /directory subfolder (so it
	  ;; doesn't have to open too many files) and org-roam here
	  ;; ignores the symlinks in the /people subfolder so it
	  ;; doesn't get duplicate IDs.
	  "^people/.+"))
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
	   :unnarrowed t)
	  ("g" "group" entry "* ${title} \n%?"
	   :target (file+head  "groups/${slug}.org"
			       ,(string-join
				 '(":PROPERTIES:"
				   ":ID: ${slug}"
				   ":END:"
				   "#+TITLE: ${title}"
				   "#+FILETAGS: :group:")
				 "\n"))
	   :unnarrowed t)
	  ("p" "project" entry "* ${title} \n%?"
	   :target (file+head  "projects/${slug}.org"
			       ,(string-join
				 '(":PROPERTIES:"
				   ":ID: ${slug}"
				   ":END:"
				   "#+TITLE: ${title}"
				   "#+FILETAGS: :project:")
				 "\n"))
	   :unnarrowed t)))
  (defun org-roam-custom-help ()
    (interactive)
    (message "[b]uffer, [f]ind, [i]nsert, [c]apture, [n]ew node, [g]raph"))
  :config
  ;; Override this function to swap underscores with dashes
  (defun jw-org-roam-node-slug-use-dashes (slug)
    (s-replace "_" "-" slug))
  (advice-add 'org-roam-node-slug :filter-return #'jw-org-roam-node-slug-use-dashes)
  (org-roam-db-autosync-mode)
  (global-set-key (kbd "C-x C-M-f") 'org-roam-node-find)
  :bind (:map org-mode-map
	      ("C-c b" . org-roam-buffer-toggle)
	      ("C-c f" . org-roam-node-find)
	      ("C-x f" . org-roam-node-find) ;; accidentally keep hitting this one
	      ("C-c i" . org-roam-node-insert)
	      ("C-c c" . org-roam-capture)
	      ("C-c n" . org-id-get-create)
	      ("C-c g" . org-roam-graph)
	      ("C-c h" . org-roam-custom-help)
	      ("M-."   . org-open-at-point)))

(use-package org-roam-ui
  :ensure t
  :init
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-mode nil))
  (add-to-list 'desktop-minor-mode-table
               '(org-roam-ui-follow-mode nil)))

(use-package org-superstar
  :ensure t
  :init
  (setq  org-superstar-special-todo-items t)
  :hook (org-mode))

(use-package powerthesaurus
  :ensure t
  :bind (:map org-mode-map
	      ("C-M-:" .     powerthesaurus-lookup-synonyms-dwim)
	      ("C-u C-M-:" . powerthesaurus-lookup-dwim)))
