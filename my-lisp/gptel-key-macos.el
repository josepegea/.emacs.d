(require 'macos-keychain)

(setq gptel-api-key (get-password-from-keychain "openai-api-key"))
(provide 'gptel-key-macos)
