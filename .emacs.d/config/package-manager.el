(provide 'package-manager)

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))

;(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)

;; inspired by https://github.com/rexim/emacs.rc/blob/master/.emacs.rc/package-manager-rc.el

(defvar package-manager-refreshed nil)

(defun package-manager-refresh-once ()
  (when (not package-manager-refreshed)
    (setq package-manager-refreshed t)
    (package-refresh-contents)))

(defun ensure-packages-installed (&rest packages)
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-manager-refresh-once)
      (package-install package))))

(package-initialize)

(setq package-enable-at-startup nil)
