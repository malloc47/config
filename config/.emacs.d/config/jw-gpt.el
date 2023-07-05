(provide 'jw-gpt)

(use-package chatgpt-shell
  :ensure t
  :config
  (setq chatgpt-shell-openai-key
	(lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))

(use-package dall-e-shell
  :ensure t
  :config
  (setq dall-e-shell-openai-key
	(lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))
