(provide 'theme)

(ensure-packages-installed 'solarized-theme 'zenburn-theme)

;; turn off window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (display-graphic-p)
  ;; load color theme
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t))

(defun new-frame-setup (frame)
  (when (display-graphic-p frame)
    (setq solarized-distinct-fringe-background t)
    (load-theme 'solarized-light t)))
(add-hook 'after-make-frame-functions 'new-frame-setup)

(global-display-line-numbers-mode)
