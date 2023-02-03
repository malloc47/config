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
  :bind ("C-c m c" . 'mc/edit-lines))

(use-package markdown-mode
  :init
  (setq markdown-command "pandoc --from=markdown --to=html"))

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
