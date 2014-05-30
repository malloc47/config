(provide 'theme)

;; turn off window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)

;; load color theme
(load-theme 'solarized-light t)
