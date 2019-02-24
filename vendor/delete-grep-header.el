;; Hide first lines of rgrep, projectile-find-in-project, etc
;; See http://stackoverflow.com/a/16133543

(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defvar delete-grep-header-advice
  (ad-make-advice
   'delete-grep-header nil t
   '(advice lambda () (delete-grep-header))))

(defun add-delete-grep-header-advice (function)
  (ad-add-advice function delete-grep-header-advice 'after 'first)
  (ad-activate function))

(mapc 'add-delete-grep-header-advice
      '(grep lgrep grep-find rgrep zrgrep))

(provide 'delete-grep-header)
