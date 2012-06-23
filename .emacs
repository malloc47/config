(setq load-path (append load-path '("~/.emacs.d/")))

;;; Autogenerated font info
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "Terminus")))))

(setq default-frame-alist '((font . "Terminus-11")))

;; (set-face-attribute 'default nil :font "-xos4-Terminus-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
;; (set-face-attribute 'default nil :height 110)

(setq window-system-default-frame-alist
      '(;; if frame created on x display
        (x
	 (menu-bar-lines . nil) (tool-bar-lines . nil)
	 (mouse-wheel-mode . 1)
	 (mouse-wheel-follow-mouse . t)
	 (mouse-avoidance-mode . 'exile))
        ;; if on term
        (nil
	 (menu-bar-lines . 0) (tool-bar-lines . 0)
	 (background-color . "black")
	 (foreground-color . "white"))))

;;; Recompile .emacs on demand
(require 'bytecomp)
(defun autocompile nil
  ;;  (interactive)
  (if (string= (buffer-file-name)
	       (expand-file-name (concat default-directory ".emacs")))
      (if (byte-compile-file (buffer-file-name))
	  (run-at-time 1 nil
		       (lambda () (delete-windows-on "*Compile-Log*"))))))
(add-hook 'after-save-hook 'autocompile)

;;; keybindings
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)
;; (windmove-default-keybindings)
;; (global-set-key [M-left] 'windmove-left)          ; move to left windnow
;; (global-set-key [M-right] 'windmove-right)        ; move to right window
;; (global-set-key [M-up] 'windmove-up)              ; move to upper window
;; (global-set-key [M-down] 'windmove-down)          ; move to downer window
(global-set-key (kbd "M-h") 'windmove-left)          ; move to left windnow
(global-set-key (kbd "M-l") 'windmove-right)        ; move to right window
(global-set-key (kbd "M-k") 'windmove-up)              ; move to upper window
(global-set-key (kbd "M-j") 'windmove-down)          ; move to downer window
(global-set-key (kbd "M-p") 'mark-paragraph)

;;; Setup colors
(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)

(load-library "matlab-load")

;;; Fixes issues with color in the shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; Scroll line-by-line
;;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t
  scroll-margin 3
  scroll-step 1
  scroll-conservatively 10
  scroll-preserve-screen-position 1)

;;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(custom-set-variables
 '(bibtex-entry-format (quote (opts-or-alts required-fields numerical-fields page-dashes whitespace realign delimiters unify-case braces)))
 '(bibtex-maintain-sorted-entries t)
 '(blink-cursor-mode nil)
 '(color-theme-is-global t)
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(global-font-lock-mode t)
 '(haskell-font-lock-symbols t)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(icomplete-mode t)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((menu-bar-lines . 0) (tool-bar-lines . 0))))
 '(scroll-bar-mode nil)
 '(set-fill-column 80)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
 ;; Get rid of tooltips

(global-auto-revert-mode t)

;; Ammend the auto-mode-alist with new aliases for appropriate modes
(setq auto-mode-alist
      (append '(("\\.sc$" . scheme-mode)
		("\\.html$" . html-mode)
		("\\.h$" . c++-mode)
		("\\.pl$" . prolog-mode))
	      auto-mode-alist))

;;; Fix annoyance
(put 'downcase-region 'disabled nil)

;; Scheme Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'scheme)
(require 'cc-mode)
 
;(setq scheme-program-name "run-isci -sch 300 -scmh 500")
;(setq scheme-program-name "qsci")
(setq scheme-program-name "~/darpa-collaboration/bin/darpa-wrap ~/darpa-collaboration/bin/dsci -sch 2000 -scmh 2500")

(defun scheme2c-abort () (interactive) (comint-send-string (scheme-proc) ":a\n"))
(defun scheme2c-back-trace () (interactive) (comint-send-string (scheme-proc) ":b\n"))
(defun scheme2c-continue () (interactive) (comint-send-string (scheme-proc) ":c\n"))
(defun scheme2c-to-caller () (interactive) (comint-send-string (scheme-proc) ":n\n"))
(defun scheme2c-from-caller () (interactive) (comint-send-string (scheme-proc) ":p\n"))
(defun scheme2c-up () (interactive) (comint-send-string (scheme-proc) ":<\n"))
(defun scheme2c-down () (interactive) (comint-send-string (scheme-proc) ":>\n"))

(setq c-indent-level 1)
(setq c-continued-statement-offset 0)
(setq c-brace-offset 0)
(setq c-argdecl-indent 0)
(setq c-label-offset 0)
(setq lisp-body-indent 1)
(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'eval-when 'scheme-indent-function 1)
(put 'with-font 'scheme-indent-function 1)
(put 'call-with-postscript-file 'scheme-indent-function 1)
(put 'parallel-do 'scheme-indent-function 2)
(define-key scheme-mode-map "\r" 'newline-and-indent)
(define-key scheme-mode-map "\C-zl" 'load-file-lisp)
(define-key scheme-mode-map "\C-zc" 'eval-defun-lisp)
(define-key scheme-mode-map "\C-ze" 'eval-defun-lisp)
(define-key scheme-mode-map "\C-z\C-c" 'eval-defun-and-go-lisp)
(define-key scheme-mode-map "\C-z\C-e" 'eval-defun-and-go-lisp)
(define-key scheme-mode-map "\C-z)" 'find-unbalanced-lisp)

(require 'cmuscheme)
(define-key inferior-scheme-mode-map "\M-a" 'scheme2c-abort)
(define-key inferior-scheme-mode-map "\M-b" 'scheme2c-back-trace)
(define-key inferior-scheme-mode-map "\M-c" 'scheme2c-continue)
;; M-c is not a prefix in emacs23, so these keybindings are slightly different
(define-key inferior-scheme-mode-map "\C-cn" 'scheme2c-to-caller) ; "M-c-n"
(define-key inferior-scheme-mode-map "\C-cp" 'scheme2c-from-caller) ; "M-c-p"
(define-key inferior-scheme-mode-map "\C-c<" 'scheme2c-up)
(define-key inferior-scheme-mode-map "\C-c>" 'scheme2c-down)
(define-key inferior-scheme-mode-map "\C-z)" 'check-parens)

;;; Python

(setenv "PYTHONPATH" ".")

;;; Highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; (put 'when 'scheme-indent-function 1)
;; (put 'unless 'scheme-indent-function 1)
;; (put 'syntax-rules 'scheme-indent-function 13)
;; (put 'eval-when 'scheme-indent-function 1)
;; (put 'with-font 'scheme-indent-function 1)
;; (put 'call-with-postscript-file 'scheme-indent-function 1)
;; (put 'parallel-do 'scheme-indent-function 2)

;;; TeX stuff
(setq latex-run-command "pdflatex")
(eval-after-load "tex" 
  '(add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t))) 
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell) 

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(load "shell-parse.el" nil t t)

;;; Life gets easier when you don't have duplicate buffer names
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(setq column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;;; Can't get by without it
(require 'ido)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
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


;;; http://www.emacswiki.org/emacs/PrettyGreek
(defun pretty-greek ()
 (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
  (loop for word in greek
	for code = 97 then (+ 1 code)
	do  (let ((greek-char (make-char 'greek-iso8859-7 code))) 
	     (font-lock-add-keywords nil
				     `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
					(0 (progn (decompose-region (match-beginning 2) (match-end 2))
						  nil)))))
	     (font-lock-add-keywords nil 
				     `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
					(0 (progn (compose-region (match-beginning 2) (match-end 2)
								  ,greek-char)
						  nil)))))))))  (add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)
(add-hook 'scheme-mode-hook 'pretty-greek)

;; TODO fix terminal bindings with this
;; (defvar real-keyboard-keys
;;   '(("M-<up>"        . "\M-[1;3A")
;;     ("M-<down>"      . "\M-[1;3B")
;;     ("M-<right>"     . "\M-[1;3C")
;;     ("M-<left>"      . "\M-[1;3D")
;;     ("C-<return>"    . "\C-j")
;;     ("C-<delete>"    . "\M-[3;5~")
;;     ("C-<up>"        . "\M-[1;5A")
;;     ("C-<down>"      . "\M-[1;5B")
;;     ("C-<right>"     . "\M-[1;5C")
;;     ("C-<left>"      . "\M-[1;5D"))
;;   "An assoc list of pretty key strings
;; and their terminal equivalents.")

;; (defun key (desc)
;;   (or (and window-system (read-kbd-macro desc))
;;       (or (cdr (assoc desc real-keyboard-keys))
;;           (read-kbd-macro desc))))

;; (global-set-key (key "M-<left>") 'windmove-left)          ; move to left windnow
;; (global-set-key (key "M-<right>") 'windmove-right)        ; move to right window
;; (global-set-key (key "M-<up>") 'windmove-up)              ; move to upper window
;; (global-set-key (key "M-<down>") 'windmove-down)
					; move to downer window
(require 'term)
(defun visit-ansi-term ()
 "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
 (interactive)
 (let ((is-term (string= "term-mode" major-mode))
       (is-running (term-check-proc (buffer-name)))
       (term-cmd "/bin/bash")
       (anon-term (get-buffer "*ansi-term*")))
  (if is-term
    (if is-running
      (if (string= "*ansi-term*" (buffer-name))
	(call-interactively 'rename-buffer)
       (if anon-term
	 (switch-to-buffer "*ansi-term*")
	(ansi-term term-cmd)))
     (kill-buffer (buffer-name))
     (ansi-term term-cmd))
   (if anon-term
     (if (term-check-proc "*ansi-term*")
       (switch-to-buffer "*ansi-term*")
      (kill-buffer "*ansi-term*")
      (ansi-term term-cmd))
    (ansi-term term-cmd)))))
(global-set-key (kbd "<f2>") 'visit-ansi-term)

(require 'org-latex)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Save session
(desktop-save-mode 1)
