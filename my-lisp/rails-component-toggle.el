(defun rails-component-toggle ()
  "Toggle between a ViewComponent Ruby file and its sidecar ERB template, handling nested subfolders."
  (interactive)
  (let* ((fname (or (buffer-file-name)
                    (user-error "No file associated with this buffer!")))
         (components-dir (locate-dominating-file fname "app/components"))
         (relative
          (when components-dir
            (file-relative-name fname (expand-file-name "app/components" components-dir)))))
    (unless (and components-dir relative)
      (user-error "Not in app/components"))
    (let* ((base
            (if (string-match "\\(.*\\)_component\\.rb\\'" relative)
                (match-string 1 relative)
              (if (string-match "\\(.*\\)_component/\\([^/]+\\)_component\\.html\\.erb\\'" relative)
                  (match-string 1 relative)
                (if (string-match "\\(.*\\)_component\\.html\\.erb\\'" relative)
                    (match-string 1 relative)
                  nil))))
           (rb-file (expand-file-name (concat "app/components/" base "_component.rb") components-dir))
           (sub-template (expand-file-name (concat "app/components/" base "_component/" (file-name-nondirectory base) "_component.html.erb") components-dir))
           (flat-template (expand-file-name (concat "app/components/" base "_component.html.erb") components-dir)))
      (cond
       ;; If we're in the .rb file
       ((string-suffix-p ".rb" fname)
        (cond
         ((file-exists-p sub-template) (find-file sub-template))
         ((file-exists-p flat-template) (find-file flat-template))
         (t (find-file sub-template))))
       ;; If we're in the subfolder template
       ((and (string-match "\\(.*\\)_component/\\([^/]+\\)_component\\.html\\.erb\\'" relative)
             (string= (match-string 2 relative) (file-name-nondirectory (match-string 1 relative))))
        (if (file-exists-p rb-file)
            (find-file rb-file)
          (message "Component Ruby file does not exist!")))
       ;; If we're in the flat template
       ((string-match "\\(.*\\)_component\\.html\\.erb\\'" relative)
        (if (file-exists-p rb-file)
            (find-file rb-file)
          (message "Component Ruby file does not exist!")))
       (t
        (message "Not a recognized ViewComponent file."))))))

(global-set-key (kbd "C-c v") #'rails-component-toggle)

(provide 'rails-component-toggle)
