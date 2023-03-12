(provide 'java-lang)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c C-l"))

(use-package lsp-ui :ensure t)

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp-deferred))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :hook (java-mode . projectile-mode))

(defun jw/java-extensions ()
  ;; Fix issue where nested pom.xml files are not recognized as
  ;; project roots before the .git repo is recognized.
  (setq-local
   projectile-project-root-functions '(projectile-root-local
				       projectile-root-marked
                                       projectile-root-top-down
                                       projectile-root-top-down-recurring
                                       projectile-root-bottom-up)))

(use-package cc-mode
  :config
  (define-key java-mode-map (kbd "C-c C-l") nil)
  :hook (java-mode . jw/java-extensions))
