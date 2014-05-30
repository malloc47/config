(provide 'line-numbers)

(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq linum-format "%3d\u2506")
