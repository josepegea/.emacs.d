(defun jes-enter-selection ()
  "Replaces isearch search string with current selection, as Cmd-e in macOS"
  (interactive)
  (setq isearch-string (buffer-substring (region-beginning) (region-end))))

(provide 'jes-enter-selection)
