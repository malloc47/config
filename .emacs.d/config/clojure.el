(provide 'clojure)

(ensure-packages-installed 'clojure-mode 'cider 'align-cljlet 'paredit 'clj-refactor)

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
	    (clj-refactor-mode 1)
	    (cljr-add-keybindings-with-prefix "C-c C-v")))

(setq cider-prompt-for-symbol nil)

(add-hook 'cider-repl-mode-hook 'paredit-mode)
