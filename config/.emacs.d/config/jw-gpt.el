(provide 'jw-gpt)

(use-package chatgpt-shell
  :ensure t
  :config
  (setq chatgpt-shell-openai-key
	(lambda ()
	  (if (string-equal "drw" (system-name))
	      (auth-source-pick-first-password :host "azure-openai.drwcloud.com")
	      (auth-source-pick-first-password :host "api.openai.com"))))
  (when (string-equal "drw" (system-name))
    (setq chatgpt-shell-api-url-base "https://azure-openai.drwcloud.com")
    (setq chatgpt-shell-api-url-path "/openai/deployments/gpt-4-turbo/chat/completions?api-version=2024-02-15-preview")
    (setq chatgpt-shell-auth-header
	  (lambda ()
	    (format "api-key: %s"
		    (auth-source-pick-first-password
		     :host
		     "azure-openai.drwcloud.com")))))
  (setq chatgpt-shell-model-versions
	'("gpt-3.5-turbo"
	  "gpt-3.5-turbo-0613"
	  "gpt-3.5-turbo-16k"
	  "gpt-3.5-turbo-16k-0613"
	  "gpt-4"
	  "gpt-4-turbo"
	  "gpt-4-0613"
	  "gpt-4-32k"
	  "gpt-4-32k-0613")))

(use-package dall-e-shell
  :ensure t
  :config
  (setq dall-e-shell-openai-key
	(lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))
