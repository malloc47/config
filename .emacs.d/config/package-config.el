(provide 'package-config)

(require 'package)
(require 'cl)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Borrowed from prelude

(defvar required-packages
  '(clojure-mode
    cider
    align-cljlet
    paredit
    markdown-mode
    magit
    find-file-in-project
    solarized-theme
    zenburn-theme)
  "Custom package list installed on launch")

(defun all-packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (all-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
