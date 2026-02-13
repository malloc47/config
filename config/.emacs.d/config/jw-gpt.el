(provide 'jw-gpt)

(use-package gptel
  :ensure t
  :config
  (cond
   ((string-equal "nylmd-jwaggon1" (system-name))
    (setq gptel-api-key (auth-source-pick-first-password :vendor "azure-openai"))
    (setq gptel-model 'claude-opus-4-5)
    (setq gptel-backend (gptel-make-anthropic
			 "Anthropic"
			 :host "aoai-gateway.drwcloud.com"
			 :endpoint "/anthropic/v1/messages"
			 :stream t
			 :key (auth-source-pick-first-password :vendor "azure-openai")
			 :models '(claude-haiku-4-5
				   claude-opus-4-5
				   claude-opus-4-6
				   claude-sonnet-4-5)))
    (gptel-make-azure "Azure"
      :protocol "https"
      :host "azure-openai.drwcloud.com"
      :endpoint "/openai/v1/chat/completions"
      :stream t
      ;; https://drw-azureai.drwcloud.com/models
      :models '(codex-mini
		codex-mini_2025-05-16
		dall-e-3
		gpt-4
		gpt-4.1
		gpt-4.1-mini
		gpt-4.1-mini_2025-04-14
		gpt-4.1-nano
		gpt-4.1-nano_2025-04-14
		gpt-4o
		gpt-4o-mini
		gpt-4o-mini-transcribe
		gpt-4o-mini-tts
		gpt-4o-mini_2024-07-18
		gpt-4o-transcribe
		gpt-4o_2024-05-13
		gpt-4o_2024-08-06
		gpt-4o_2024-11-20
		gpt-5
		gpt-5-chat
		gpt-5-chat_2025-08-07
		gpt-5-chat_2025-10-03
		gpt-5-codex
		gpt-5-mini
		gpt-5-nano
		gpt-5-nano_2025-08-07
		gpt-5-pro
		gpt-5.1
		gpt-5.1-chat
		gpt-5.1-codex
		gpt-5.1-codex-max
		gpt-5.1-codex-mini
		gpt-5.2
		gpt-5.2-chat
		gpt-5.2-codex
		gpt-image-1
		grok-4
		mistral-document-ai-2505
		model-router
		o1
		o1_2024-12-17
		o3
		o3-deep-research
		o3-mini
		o3-mini_2025-01-31
		o3-pro
		o3_2025-04-16
		o4-mini
		o4-mini_2025-04-16
		text-embedding-3-large
		text-embedding-3-small
		text-embedding-ada-002
		whisper))))

  :custom
  (gptel-default-mode 'markdown-mode))
