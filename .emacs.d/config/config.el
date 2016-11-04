(provide 'config)

(ensure-packages-installed 'markdown-mode
			   'haskell-mode
			   'find-file-in-project
			   'ido-ubiquitous)

;; smooth scrolling
(setq redisplay-dont-pause t
  scroll-margin 3
  scroll-step 1
  scroll-conservatively 10
  scroll-preserve-screen-position 1)

;; ido-mode
(require 'ido)
(setq ido-default-buffer-method 'selected-window)
(setq ido-default-file-method 'selected-window)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-max-directory-size 1000000)
(ido-mode 1)
(global-set-key
 "\M-x"
 (lambda ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (all-completions "" obarray 'commandp))))))

;; y/n vs yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; save sessions
(desktop-save-mode 1)

;; unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; find file in project
(setq ffip-full-paths t)

;; remove whitespace
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

;; ERC settings

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
