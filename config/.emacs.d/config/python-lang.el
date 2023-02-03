(provide 'python-lang)

(use-package company :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-completion-provider :capf)
  :hook (python-mode . lsp-deferred)
  ;; :config
  ;; (setq lsp-file-watch-threshold nil)
  )

(use-package lsp-pyright :ensure t)

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

(defun jw/python-extensions ()
  (python-pretty-lambda)
  (define-key python-mode-map (kbd "C-c f") 'python-shell-send-file)
  (setq company-minimum-prefix-length 1))

(use-package python-mode
  :ensure t
  :hook ((python-mode . prettify-symbols-mode)
	 (python-mode . jw/python-extensions)))
