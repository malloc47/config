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

;; base16-theme stores display-conditional face specs (truecolor GUI,
;; 256-color terminal, 16-color terminal) rather than computing one fixed
;; color set at load-theme call time.  This means a single load-theme
;; call works for all frame types simultaneously — the correct sub-spec
;; is selected per-frame at render time, so emacsclient -t and GUI frames
;; both get the right colors without daemon-specific hacks.
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-solarized-light t)
  (defun new-frame-setup (frame)
    (with-selected-frame frame
      ;; menu-bar-mode -1 at startup doesn't reliably apply to frames
      ;; created later by emacsclient -t; enforce it per-frame
      (menu-bar-mode -1)))
  (add-hook 'after-make-frame-functions 'new-frame-setup))
