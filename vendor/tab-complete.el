;; Use tab either for autocompletion or indentation
;; See https://lokeshw24.wordpress.com/2013/08/07/emacs-how-to-auto-complete-words-by-tab-key/
;;;;;;;;;;;;;;;;;
(require 'dabbrev)
(setq dabbrev-always-check-other-buffers t)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
 
(global-set-key "\C-i" 'tab-complete)
 
(defun tab-complete (&optional pre-arg)
  "If a region is active, calls indent-region. 
Else, if preceeding character is part of a word then dabbrev-expand,
else if right of non whitespace on line then tab-to-tab-stop or
indent-relative, else if last command was a tab or return then dedent
one step, else indent 'correctly'"
  (interactive "*P")
  (cond
   ;; The region is active, indent it.
   ;; Added by JES, copied from indent.el
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ;; The rest of conditions come from the original
   ((= (char-syntax (preceding-char)) ?w)
    (let ((case-fold-search t)) (dabbrev-expand pre-arg)))
   ((> (current-column) (current-indentation))
    (indent-relative))
   (t (indent-according-to-mode)))
  (setq this-command 'tab-complete))
 
(add-hook 'html-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'haml-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'web-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'css-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'js-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'coffee-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (local-set-key "\C-i"     'tab-complete)))

(provide 'tab-complete)
