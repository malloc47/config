(provide 'clojure)

(ensure-packages-installed 'clojure-mode 'cider 'align-cljlet 'paredit)

(require 'paredit)
(require 'align-cljlet)
(require 'clojure-mode)

(defun clojure-reload-buffer ()
  (interactive)
  (if (and (cider-connected-p) (string= "(ns " (buffer-substring-no-properties 1 5)))
      (cider-load-current-buffer)))

(add-hook 'clojure-mode-hook
          (lambda ()
	    (paredit-mode +1)
	    (enable-show-paren-mode)
	    (add-hook 'after-save-hook 'clojure-reload-buffer nil 'make-local)
	    (define-key clojure-mode-map (kbd "C-c C-a") 'align-cljlet)
	    (dolist (macro '(fresh conde run run* for-all for-map go go-loop
				   for> doseq> fn> defn> defprotocol> gen-for))
	      (put-clojure-indent macro 'defun))))

(add-hook 'cider-repl-mode-hook 'paredit-mode)
