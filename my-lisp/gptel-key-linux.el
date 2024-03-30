(require 'gnome-keyring)

(setq gptel-api-key (get-password-from-keyring "oai_api key"))
(provide 'gptel-key-linux)
