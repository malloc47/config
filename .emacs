(setq load-path (append load-path '("~/.emacs.d/")))

;;; Autogenerated font info
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "Terminus")))))

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

;;; Keybindings
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)
;; (windmove-default-keybindings)
(global-set-key [M-left] 'windmove-left)          ; move to left windnow
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to downer window


;;; Setup colors
(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)

;;; Fixes issues with color in the shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; Scroll line-by-line
(setq scroll-step 1 scroll-conservatively 10000)

(custom-set-variables
 '(blink-cursor-mode nil) ;; No blinking cursors
 '(color-theme-is-global t) ;; Use theme everywhere
 '(display-time-mode t) ;; Show a clock on the status bar
 '(fringe-mode 0 nil (fringe)) ;; No need for the icons for wrapping text
 '(global-font-lock-mode t) ;; Special font mode
 '(icomplete-mode t) ;; Autocomplete
 '(inhibit-startup-echo-area-message nil) ;; No startup cruft
 '(inhibit-startup-screen t) ;; "
 '(initial-frame-alist (quote ((menu-bar-lines . 0) (tool-bar-lines . 0))))
;; '(matlab-shell-command-switches "-nodesktop -nosplash")
;; '(org-replace-disputed-keys t)
 '(scroll-bar-mode nil) ;; No scroll bar
 '(set-fill-column 80) ;; Wrap column
 '(show-paren-mode t) ;; Paren matching
 '(tool-bar-mode nil) ;; Get rid of the toolbar
 '(tooltip-mode nil)) ;; Get rid of tooltips

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
(ido-mode t)
(setq ido-enable-flex-matching t)

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
