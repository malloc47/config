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

;; The Emacs daemon (started by LaunchAgent) lacks the COLORFGBG env var
;; that terminals set, so Emacs defaults to background-mode=dark for TUI
;; frames — applying dark-variant colors onto our light-background terminal.
;; Force light mode since all our terminals use solarized-light via Stylix.
(setq frame-background-mode 'light)

;; doom-themes generates display-conditional face specs (separate sub-specs
;; for GUI true-color and terminal) so a single load-theme works for all
;; frame types.
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-solarized-light t)
  (defun new-frame-setup (frame)
    (with-selected-frame frame
      ;; menu-bar-mode -1 at startup doesn't reliably apply to frames
      ;; created later by emacsclient -t; enforce it per-frame
      (menu-bar-mode -1)))
  (add-hook 'after-make-frame-functions 'new-frame-setup))
