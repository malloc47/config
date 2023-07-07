(provide 'jw-gpt)

(use-package chatgpt-shell
  :ensure t
  :config
  (setq chatgpt-shell-openai-key
	(lambda ()
          (auth-source-pick-first-password :host "api.openai.com")))
  (setq chatgpt-shell-model-versions
	'("gpt-3.5-turbo"
	  "gpt-3.5-turbo-0613"
	  "gpt-3.5-turbo-16k"
	  "gpt-3.5-turbo-16k-0613"
	  "gpt-4"
	  "gpt-4-0613"
	  "gpt-4-32k"
	  "gpt-4-32k-0613")))

(use-package dall-e-shell
  :ensure t
  :config
  (setq dall-e-shell-openai-key
	(lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))
