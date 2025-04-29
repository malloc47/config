(provide 'config)

(use-package smooth-scrolling
  :ensure t
  :init
  (setq smooth-scroll-margin 5)
  :config
  (smooth-scrolling-mode 1))

(use-package desktop
  :init
  (setq desktop-dirname             "~/.emacs.d/desktop/"
	desktop-base-file-name      "emacs.desktop"
	desktop-base-lock-name      "lock"
	desktop-path                (list desktop-dirname)
	desktop-save                t
	desktop-files-not-to-save   "^$"
	desktop-load-locked-desktop t)
  :config
  (desktop-save-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package find-file-in-project
  :ensure t
  :bind ("C-x C-S-f" . 'find-file-in-project))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . 'mc/edit-lines)
	 ("C-c m n" . 'mc/mark-next-like-this)
	 ("C-c m p" . 'mc/unmark-next-like-this)
	 ("C-c m P" . 'mc/mark-previous-like-this)
	 ("C-c m N" . 'mc/unmark-previous-like-this)
	 (:repeat-map jw/mc-repeat-map
           ("n" . mc/mark-next-like-this)
	   ("p" . mc/unmark-next-like-this)
	   ("P" . mc/mark-previous-like-this)
	   ("N" . mc/unmark-previous-like-this)))
  :hook repeat-mode)

(use-package visual-fill-column
  :ensure t
  :init
  (setq-default fill-column 80)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

(defun jw/markdown-extensions ()
  (set-face-attribute 'markdown-link-face nil :inherit 'fixed-pitch))

(use-package markdown-mode
  :init
  (setq markdown-command "pandoc --from=markdown --to=html")
  (setq markdown-css-paths '("https://classless.de/classless.css"))
  (setq markdown-xhtml-header-content
      "<style type='text/css'>
body { max-width: 80%; }
</style>")
  :hook ((markdown-mode . visual-line-mode)
	 (markdown-mode . visual-fill-column-mode)
	 (markdown-mode . flyspell-mode)
	 (markdown-mode . jw/markdown-extensions))
  :bind (:map markdown-mode-map
	      ("C-u" . universal-argument)))

(use-package markdown-toc :ensure t)

(use-package synosaurus
  :ensure t
  :bind (:map markdown-mode-map
	      ("C-M-:" .     synosaurus-choose-and-replace)
	      ("C-u C-M-:" . synosaurus-lookup)))

(use-package mermaid-mode :ensure t)

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; dired open files with F
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

;; y/n vs yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; move customize to separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defun ssh-add () (interactive) (shell-command "ssh-add"))

;; Favor opening new windows side-by-side rather than top-to-bottom
(use-package window
  :init
  (setq split-width-threshold 160)
  (setq split-height-threshold nil))

(use-package flyspell
  :hook (prog-mode . flyspell-prog-mode)
  :init
  (setq flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-:" . flyspell-correct-wrapper)))

(use-package all-the-icons :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :init
  (setq all-the-icons-dired-monochrome nil)
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :after all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(defun set-region-writeable (begin end)
  "Removes the read-only text property from the marked region."
  ;; See https://stackoverflow.com/questions/7410125
  (interactive "r")
  (with-silent-modifications
    (remove-text-properties begin end '(read-only t))))
