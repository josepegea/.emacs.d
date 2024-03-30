;; Command keys that come "for-free" on macOS and that need to be defined on Linux
(message "Linux command keys")
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-g") 'isearch-repeat-forward)
(global-set-key (kbd "s-n") 'make-frame)

(global-set-key (kbd "s-c") 'cua-copy-region)
(global-set-key (kbd "s-x") 'cua-cut-region)
(global-set-key (kbd "s-v") 'cua-paste)
(global-set-key (kbd "s-a") 'mark-whole-buffer)

(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

(global-set-key (kbd "s-t") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-l") 'goto-line)

(provide 'command-keys-for-linux)
