(provide 'theme)

(ensure-packages-installed 'solarized-theme 'zenburn-theme)

;; turn off window decorations
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; show fringe on right for git-gutter-fringe
(fringe-mode '(0 . 8))

(setq solarized-distinct-fringe-background t)

;; load color theme
(load-theme 'solarized-light t)

;; line numbers
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq linum-format "%3d ")
