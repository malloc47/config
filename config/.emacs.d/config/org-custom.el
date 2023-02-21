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
     (shell . t)))
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
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
		 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
								 (string-glyph-decompose s)))))
		 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
	;; Changes here:
	(let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
			("--*" . "-")                   ;; remove sequential underscores
			("^-" . "")                     ;; remove starting underscore
			("-$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))
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
