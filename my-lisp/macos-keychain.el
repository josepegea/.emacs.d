(defun get-password-from-keychain (account)
  "Retrieves a password from macOS Keychain using `security` command."
  (let ((command (concat "security find-generic-password -a " account " -w")))
    (string-trim-right (shell-command-to-string command))))

(provide 'macos-keychain)
