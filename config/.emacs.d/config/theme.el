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

;; The Emacs daemon (started by LaunchAgent) lacks COLORFGBG and COLORTERM.
;; Without COLORFGBG, Emacs defaults to background-mode=dark; without
;; COLORTERM, emacsclient TUI frames use 256-color mode instead of 24-bit
;; truecolor — approximating hex colors through xterm-256 indices instead
;; of sending them directly.  All our terminals (Ghostty) use solarized-light
;; via Stylix and support truecolor.
(setq frame-background-mode 'light)
(unless (getenv "COLORTERM")
  (setenv "COLORTERM" "truecolor"))

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
