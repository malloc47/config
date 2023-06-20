(add-to-list 'load-path (concat user-emacs-directory
				(convert-standard-filename "config/")))

(let ((default-directory (concat user-emacs-directory
				 (convert-standard-filename "packages/"))))
  (normal-top-level-add-subdirs-to-load-path))

;; Load package-related utilities for other libraries
(require 'package-manager)

(require 'fonts)
(require 'jw-completion)
(require 'modeline)
(require 'config)
(require 'theme)
(require 'backup)
(require 'version-control)
(require 'lisp)
(require 'clojure)
(require 'elisp)
(require 'python-lang)
(require 'scala)
(require 'haskell)
(require 'keys)
(require 'osx)
(require 'typescript)
(require 'org-custom)
(require 'nix-lang)
(require 'yaml-lang)
(require 'java-lang)
(require 'jw-gpt)
