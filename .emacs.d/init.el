(add-to-list 'load-path (concat user-emacs-directory
				(convert-standard-filename "config/")))

(require 'package-config)
(require 'global)
(require 'theme)
(require 'keys)
(require 'line-numbers)
(require 'backup)
(require 'vc)
(require 'lisp)
(require 'clojure)
(require 'elisp)
