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

;; The Emacs daemon (started by LaunchAgent) lacks COLORTERM.
;; Without it, emacsclient TUI frames use 256-color mode instead of 24-bit
;; truecolor.  All our terminals (Ghostty) support truecolor.
(unless (getenv "COLORTERM")
  (setenv "COLORTERM" "truecolor"))

(use-package base16-theme
  :ensure t
  :config
  (require 'stylix-theme)  ;; nix-generated: defines base16-solarized-{light,dark}

  (defun jw/current-theme-mode ()
    "Read current theme mode from ~/.config/theme-mode, default light."
    (let ((f (expand-file-name "~/.config/theme-mode")))
      (if (file-exists-p f)
          (string-trim (with-temp-buffer (insert-file-contents f) (buffer-string)))
        "light")))

  (defun jw/apply-theme-mode (mode)
    "Apply MODE (\"light\" or \"dark\") base16 solarized theme."
    (mapc #'disable-theme custom-enabled-themes)
    (let ((theme (intern (concat "base16-solarized-" mode))))
      (setq frame-background-mode (intern mode))
      (load-theme theme t)
      ;; TUI frames: use the terminal's own bg/fg rather than
      ;; approximating hex colors through 256-color indices.
      (dolist (frame (frame-list))
        (unless (display-graphic-p frame)
          (with-selected-frame frame
            (set-face-attribute 'default frame
                                :background "unspecified-bg"
                                :foreground "unspecified-fg"))))))

  (defun jw/toggle-theme ()
    "Toggle between light and dark solarized themes."
    (interactive)
    (let* ((cur (jw/current-theme-mode))
           (new (if (string= cur "dark") "light" "dark"))
           (f (expand-file-name "~/.config/theme-mode")))
      (with-temp-file f (insert new))
      (jw/apply-theme-mode new)))

  ;; Apply theme on startup
  (setq base16-theme-256-color-source 'colors)
  (jw/apply-theme-mode (jw/current-theme-mode))

  ;; New TUI frames get the same treatment
  (defun new-frame-setup (frame)
    (with-selected-frame frame
      (menu-bar-mode -1)
      (unless (display-graphic-p frame)
        (set-face-attribute 'default frame
                            :background "unspecified-bg"
                            :foreground "unspecified-fg"))))
  (add-hook 'after-make-frame-functions 'new-frame-setup))
