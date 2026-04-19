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

;; doom-themes generates true display-conditional face specs — each face
;; has separate sub-specs for GUI true-color and terminal, chosen at
;; render time per-frame.  doom-solarized-light has carefully tuned
;; terminal face assignments so TUI mode looks close to GUI without
;; daemon-specific load-time hacks.
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
