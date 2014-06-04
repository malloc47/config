(provide 'theme)

(ensure-packages-installed 'solarized-theme 'zenburn-theme)

;; turn off window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)

;; load color theme
(load-theme 'solarized-light t)

;; line numbers
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq linum-format "%3d\u2506")
