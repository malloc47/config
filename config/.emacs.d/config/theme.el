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

(use-package solarized-theme
  :ensure t
  :init
  (setq solarized-distinct-fringe-background t)
  :config
  (defun new-frame-setup (frame)
    (with-selected-frame frame
      ;; menu-bar-mode -1 at startup doesn't reliably apply to frames
      ;; created later by emacsclient -t; enforce it per-frame
      (menu-bar-mode -1)
      (load-theme 'solarized-light t)))
  (add-hook 'after-make-frame-functions 'new-frame-setup)
  ;; In daemon mode, defer theme loading to new-frame-setup so the theme
  ;; detects the frame type (GUI vs TUI) correctly at frame-creation time.
  ;; Loading here during daemon startup can lock in GUI-mode color specs
  ;; before any TUI frame exists, producing wrong terminal colors.
  (unless (daemonp)
    (load-theme 'solarized-light t)))
