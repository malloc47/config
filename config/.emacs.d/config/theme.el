(provide 'theme)

;; turn off window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode)

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
