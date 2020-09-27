(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun jes-ng2-check-alternate-name (file orig alt)
   (and (string-suffix-p orig file) (file-exists-p (replace-in-string orig alt file))))

(defun jes-ng2--alternate-name (file)
  (cond
   ((jes-ng2-check-alternate-name file "component.ts" "template.html") (replace-in-string "component.ts" "template.html" file))
   ((jes-ng2-check-alternate-name file "component.ts" "component.html") (replace-in-string "component.ts" "component.html" file))
   ((string-suffix-p "component.ts" file) (replace-in-string "component.ts" "template.html" file))
   ((string-suffix-p "template.html" file) (replace-in-string "template.html" "component.ts" file))
   ((string-suffix-p "component.html" file) (replace-in-string "component.html" "component.ts" file))
   (t file)))
   
(defun jes-ng2-alternate ()
  "Alternates between 'template' and 'component' files in Angular2."
  (interactive)
  (find-file (jes-ng2--alternate-name (buffer-file-name))))

(provide 'jes-ng2)
