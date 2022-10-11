(provide 'python-lang)

(ensure-packages-installed 'python-mode 'lsp-pyright)

(require 'lsp-mode)
(require 'lsp-pyright)

;;; hedging my bets on switching to lsp-python-ms
;; (require 'lsp-python-ms)
;; (setq lsp-python-ms-auto-install-server t)
;; (setq lsp-python-ms-executable (executable-find "python-language-server"))


(setq lsp-completion-provider :capf)

(defun python-pretty-lambda ()
  (setq prettify-symbols-alist
        '(("lambda" . ?λ)
	  ("sum"    . ?Σ)
	  ("="      . ?←)
	  ("=="     . ?≡)
	  ("!="     . ?≠)
	  ("<="     . ?≤)
	  (">="     . ?≥)
	  ("for"    . ?∀)
	  ("if"     . ?∃)
	  ("not"    . ?¬)
	  ("or"     . ?∨)
	  ("and"    . ?∧)
	  ("True"   . ?⊤)
	  ("False"  . ?⊥)
	  ("None"   . ?∅)
	  ("self"   . ?◎)
	  ("import" . ?≺)
	  (":"      . ?▶)
	  ("\\"     . ?…)
	  ("def"    . ?§)
	  ("class"  . ?⌘)
	  ("return" . ?◀)
	  ("*"      . ?×)
	  ("^"      . ?⊕)
	  ("/"      . ?÷))))

(add-hook 'python-mode-hook
          (lambda ()
	    (python-pretty-lambda)
	    (prettify-symbols-mode +1)
	    (lsp-deferred)))
