(defun jes-enter-selection ()
  "Replaces isearch search string with current selection, as Cmd-e in macOS"
  (interactive)
  (setq isearch-string (buffer-substring (region-beginning) (region-end)))
  (isearch-update-ring (buffer-substring (region-beginning) (region-end)))
  (cua-set-mark))

(defun jes-projectile-grep ()
  "Performs a projectile grep with the current isearch string"
  (interactive)
  (projectile-grep isearch-string))

(provide 'jes-macos-bindings)
