(provide 'terminal)

;; Kitty Keyboard Protocol support
;; Enables full modifier+key pass-through when connected to a KKP-capable
;; terminal (Ghostty, Kitty, WezTerm, Alacritty >=0.13). No-ops gracefully
;; in GUI frames or terminals without KKP support.
(use-package kkp
  :ensure t
  :config
  (global-kkp-mode +1))

;; Guard packages that only work in graphical frames
(unless (display-graphic-p)
  ;; all-the-icons renders as broken glyphs in TUI
  (with-eval-after-load 'all-the-icons
    (setq all-the-icons-color-icons nil))
  (with-eval-after-load 'all-the-icons-dired
    (remove-hook 'dired-mode-hook #'all-the-icons-dired-mode))
  (with-eval-after-load 'all-the-icons-ibuffer
    (remove-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode))
  (with-eval-after-load 'all-the-icons-completion
    (all-the-icons-completion-mode -1)))
