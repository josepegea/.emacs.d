(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun jes-ng2--alternate-name (file)
  (cond
   ((string-suffix-p "component.ts" file) (replace-in-string "component.ts" "template.html" file))
   ((string-suffix-p "template.html" file) (replace-in-string "template.html" "component.ts" file))
   (t file)))
   
(defun jes-ng2-alternate ()
  "Alternates between 'template' and 'component' files in Angular2."
  (interactive)
  (find-file (jes-ng2--alternate-name (buffer-file-name))))

(provide 'jes-ng2)
