(provide 'clojure)

(use-package paredit
  :ensure
  :hook (clojure-mode cider-repl-mode inf-clojure-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package align-cljlet :ensure t)

(defun jw/clojure-extensions ()
  (enable-show-paren-mode)
  (cljr-add-keybindings-with-prefix "C-c C-v")
  ;; Move to align-cljlet :hook?
  (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
  (dolist (macro '(fresh conde run run* for-all for-map go go-loop
			 for> doseq> fn> defn> defprotocol> gen-for
			 ANY DELETE GET HEAD OPTIONS PATCH POST PUT))
    (put-clojure-indent macro 'defun))
  (local-set-key (kbd "RET") 'newline-and-indent))

(use-package inf-clojure
  :ensure t
  :hook (inf-clojure-mode . enable-show-paren-mode))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . jw/clojure-extensions))

(use-package cider
  :ensure t
  :hook clojure-mode
  :config
  (setq cider-prompt-for-symbol nil))


(use-package clj-refactor
  :ensure t
  :hook clojure-mode)

(fset 'clj-align-ns
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 67108896 134217788 19 58 114 101 113 117 105 114 101 return 67108896 19 41 2 134217848 97 108 105 103 110 45 114 101 103 101 tab 58 97 115 backspace backspace return return 67108911 21 67108896 21 67108896 21 67108896] 0 "%d")) arg)))
