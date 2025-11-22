;; Open files in dired mode using 'open'
;; See http://jblevins.org/log/dired-open
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (call-process "xdg-open" nil 0 nil fn))))))

(provide 'linux-open)
