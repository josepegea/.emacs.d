(defun get-password-from-keyring (path)
  "Retrieves a password from GNOME Keyring using `secreet-tool` command."
  (let ((command (concat "secret-tool lookup " path)))
    (string-trim-right (shell-command-to-string command))))

(provide 'gnome-keyring)
