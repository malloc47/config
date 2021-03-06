(add-to-list 'load-path (concat user-emacs-directory
				(convert-standard-filename "config/")))

;; Load package-related utilities for other libraries
(require 'package-manager)

(require 'fonts)
(require 'config)
(require 'theme)
(require 'backup)
(require 'version-control)
(require 'lisp)
(require 'clojure)
(require 'elisp)
(require 'python)
(require 'scala)
(require 'haskell)
(require 'email)
(require 'chat)
(require 'keys)
(require 'osx)
(require 'typescript)
(require 'org-custom)
(require 'anki)
