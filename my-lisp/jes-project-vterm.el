(require 'vterm)

(defun my/project-root ()
  "Return the project root using projectile or project.el."
  (or (and (fboundp 'projectile-project-root)
           (projectile-project-root))
      (when (fboundp 'project-current)
        (when-let ((proj (project-current)))
          (car (project-roots proj))))))

(defun my/project-name ()
  "Return the project name using projectile or project.el."
  (or (and (fboundp 'projectile-project-name)
           (projectile-project-name))
      (when (fboundp 'project-current)
        (when-let ((proj (project-current)))
          (file-name-nondirectory
           (directory-file-name
            (car (project-roots proj))))))))

(defun my/project-vterm (idx)
  "Jump to or create project vterm buffer number IDX (1-9) at project root."
  (interactive "P")
  (let* ((project-root (my/project-root))
         (project-name (my/project-name))
         (num (if (numberp idx)
                  idx
                (string-to-number (car (last (this-command-keys-vector))))))
         (buf-name (format "*vterm: %s-%d*" project-name num)))
    (unless project-root
      (error "Not in a project"))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
      (let ((default-directory project-root))
        (vterm buf-name)))))

;; Optional keybindings for number keys 1-9
(dotimes (i 9)
  (let ((n (+ 1 i)))
    (global-set-key (kbd (format "M-s-v %d" n))
                    `(lambda () (interactive) (my/project-vterm ,n)))))

(provide 'jes-project-vterm)
