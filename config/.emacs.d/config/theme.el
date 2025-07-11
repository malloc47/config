(provide 'theme)

;; turn off window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq initial-buffer-choice nil)
(setq indicate-empty-lines nil)

(use-package nano
  :no-require t
  :load-path "packages/nano-emacs/"
  :config
  (require 'nano-faces)
  (require 'nano-theme)
  (require 'nano-theme-dark)
  (require 'nano-theme-light)
  (nano-theme-set-light)
  (require 'nano-modeline)
  (nano-faces)
  (nano-theme))

;; Load nano for layout and fonts, and then let solarize override

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  :config
  (when (display-graphic-p)
    (load-theme 'solarized-light t))
  (defun new-frame-setup (frame)
    (when (display-graphic-p frame)
      (load-theme 'solarized-light t)))
  (add-hook 'after-make-frame-functions 'new-frame-setup))
