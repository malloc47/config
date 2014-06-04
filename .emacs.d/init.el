(add-to-list 'load-path (concat user-emacs-directory
				(convert-standard-filename "config/")))

;; Load package-related utilities for other libraries
(require 'package-manager)

(require 'config)
(require 'theme)
(require 'backup)
(require 'vc)
(require 'lisp)
(require 'clojure)
(require 'elisp)
(require 'keys)
