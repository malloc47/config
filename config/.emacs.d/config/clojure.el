(provide 'clojure)

(ensure-packages-installed 'clojure-mode 'cider 'align-cljlet 'paredit inf-clojure)

(require 'paredit)
(require 'align-cljlet)
(require 'clojure-mode)

(add-hook 'clojure-mode-hook
          (lambda ()
	    (paredit-mode +1)
	    (enable-show-paren-mode)
	    (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
	    (dolist (macro '(fresh conde run run* for-all for-map go go-loop
				   for> doseq> fn> defn> defprotocol> gen-for))
	      (put-clojure-indent macro 'defun))
	    (cljr-add-keybindings-with-prefix "C-c C-v")
	    (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'inf-clojure-mode-hook
          (lambda ()
	    (paredit-mode +1)
	    (enable-show-paren-mode)))

(setq cider-prompt-for-symbol nil)

(add-hook 'cider-repl-mode-hook 'paredit-mode)

(fset 'clj-align-ns
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 67108896 134217788 19 58 114 101 113 117 105 114 101 return 67108896 19 41 2 134217848 97 108 105 103 110 45 114 101 103 101 tab 58 97 115 backspace backspace return return 67108911 21 67108896 21 67108896 21 67108896] 0 "%d")) arg)))

(setq cljr-auto-sort-ns nil)
