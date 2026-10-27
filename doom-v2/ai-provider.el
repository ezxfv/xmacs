;;; ai-provider.el -*- lexical-binding: t; -*-

(require 'json)

(defconst eden-sub2api-base-url "https://sub2api.fibbery.top"
  "Base URL for the OpenAI-compatible Sub2API relay.")

(defconst eden-sub2api-default-model 'gpt-6-luna
  "Default model shared by interactive AI integrations.")

(defconst eden-sub2api-models
  '(gpt-5.6
    gpt-5.6-luna
    gpt-5.6-sol
    gpt-5.6-terra
    gpt-6
    gpt-6-astra
    gpt-6-luna
    gpt-6-sol
    gpt-6.1-sol)
  "GPT models at version 5.6 or newer advertised by Sub2API.")

(defun eden-sub2api-token ()
  "Return `SUB2API_TOKEN' without persisting it in Emacs configuration."
  (or (getenv "SUB2API_TOKEN")
      (user-error "SUB2API_TOKEN is not available to Emacs")))

(defun eden-sub2api-codex-config ()
  "Return isolated Codex configuration for Sub2API as JSON."
  (json-serialize
   `((model . ,(symbol-name eden-sub2api-default-model))
     (model_provider . "sub2api")
     (model_reasoning_effort . "none")
     (disable_response_storage . t)
     (model_providers
      . ((sub2api
          . ((name . "Sub2API")
             (base_url . ,(concat eden-sub2api-base-url "/v1"))
             (wire_api . "responses")
             (env_key . "SUB2API_TOKEN")
             (requires_openai_auth . :false))))))))

(provide 'ai-provider)
