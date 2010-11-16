;(setq viper-mode t)
;(require 'viper)
;(load "vip")
;(setq term-setup-hook 'vip-mode)

(load "scheme")
(setq scheme-program-name "qsci")

;;; Always do syntax highlighting
(global-font-lock-mode 1)

;;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;(require 'quack)

;(setq stack-trace-on-error t)

;;; Turn off menu bar

;(menu-bar-mode -1)
(mouse-wheel-mode 1)
;(tool-bar-mode -1)

(put 'when 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'eval-when 'scheme-indent-function 1)
(put 'with-font 'scheme-indent-function 1)
(put 'call-with-postscript-file 'scheme-indent-function 1)
(put 'parallel-do 'scheme-indent-function 2)
