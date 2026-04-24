(provide 'jw-ai)

(use-package gptel
  :ensure t
  :config
  (when (getenv "GPTEL_ANTHROPIC_HOST")
    (setq gptel-api-key (auth-source-pick-first-password :vendor "azure-openai"))
    (setq gptel-model 'claude-opus-4-5)
    (setq gptel-backend (gptel-make-anthropic
			 "Anthropic"
			 :host (getenv "GPTEL_ANTHROPIC_HOST")
			 :endpoint "/anthropic/v1/messages"
			 :stream t
			 :key (auth-source-pick-first-password :vendor "azure-openai")
			 :models '(claude-haiku-4-5
				   claude-opus-4-5
				   claude-opus-4-6
				   claude-sonnet-4-5))))
  (when (getenv "GPTEL_AZURE_HOST")
    (gptel-make-azure "Azure"
      :protocol "https"
      :host (getenv "GPTEL_AZURE_HOST")
      :endpoint "/openai/v1/chat/completions"
      :stream t
      :models '(gpt-4o gpt-4o-mini o3 o3-mini o4-mini)))

  :custom
  (gptel-default-mode 'markdown-mode))

(use-package shell-maker :ensure t)

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))
