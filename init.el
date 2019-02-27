;; Keys
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(setq mac-command-modifier 'super)

(add-to-list 'load-path "~/.emacs.d/my-lisp")

(add-to-list 'load-path "~/.emacs.d/vendor/aquamacs")
(require 'emulate-mac-keyboard-mode)
(emulate-mac-spanish-keyboard-mode)

;; Basic macOS keys
(global-set-key [s-M-left] 'previous-buffer)
(global-set-key [s-M-right] 'next-buffer)
(global-set-key (kbd "s-<") 'other-frame)
(global-set-key [s-right] 'end-of-line)
(global-set-key [s-left] 'beginning-of-line)
(global-set-key [s-down] 'end-of-buffer)
(global-set-key [s-up] 'beginning-of-buffer)

;; macOS like search and replace
(require 'jes-macos-bindings)
                                                                  ;; "Cmd-f" Find -> Works as is
                                                                  ;; "Cmd-g" Find next -> Works as is
(global-set-key (kbd "s-e") 'jes-enter-selection)                 ;; "Cmd-e" Enter selection for next finds
(global-set-key (kbd "s-G") 'isearch-repeat-backward)             ;; "Cmd-G" Find previous
(define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill)      ;; "Cmd-v" When isearching pastes the normal clipboard contents
(global-set-key (kbd "s-r") 'isearch-query-replace)               ;; "Cmd-r" Replace with current isearch string
(global-set-key (kbd "s-R") 'isearch-occur)                       ;; "Cmd-R" Occur with current isearch string
(global-set-key (kbd "s-F") 'jes-projectile-grep)                 ;; "Cmd-F" Projectile grep with current isearch string
(global-set-key (kbd "<C-s-268632070>") 'projectile-grep)         ;; "C-Cmd-f" Projectile grep (nicer, shorter shortcut)

;; Easier window navigation
(global-set-key [C-s-M-left] 'windmove-left)
(global-set-key [C-s-M-right] 'windmove-right)
(global-set-key [C-s-M-up] 'windmove-up)
(global-set-key [C-s-M-down] 'windmove-down)


;; Define package sources

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)

(setq package-enable-at-startup nil)

;; Autoload packages

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Controlled by custom. Don't touch

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (wombat)))
 '(ns-alternate-modifier (quote meta))
 '(package-selected-packages
   (quote
    (yaml-mode inf-ruby auto-dim-other-buffers auto-dim-other-buffers-mode undo-tree multiple-cursors rspec-mode rvm magit ido-vertical-mode flx-ido projectile coffee-mode js2-mode haml-mode web-mode exec-path-from-shell use-package)))
 '(safe-local-variable-values (quote ((rspec-spec-command . "rspec -Ispec/app")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Window title
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Use normal PATH in OSX

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

;; Enable CUA mode
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;; Some sensible defaults
(show-paren-mode 1)
(blink-cursor-mode 1)
(savehist-mode 1)
(set-default 'cursor-type '(bar . 2))

;; Enable TextMate mode
(add-to-list 'load-path "~/.emacs.d/vendor/textmate.el")
(require 'textmate)
(textmate-mode)

;; Avoid key conflicts between Org and CUA modes
(setq org-replace-disputed-keys t)
(defun my-org-mode-hook ()
  "Hook for Org mode tweaks."
  (setq org-replace-disputed-keys t)
  (define-key org-mode-map [M-left] nil)
  (define-key org-mode-map [M-right] nil)
  (define-key org-mode-map [S-M-left] nil)
  (define-key org-mode-map [S-M-right] nil)
)
(add-hook 'org-mode-hook  'my-org-mode-hook)

;; The same for Markdown
(defun my-markdown-mode-hook ()
  "Hook for Markdown mode tweaks."
  (define-key markdown-mode-map [M-left] nil)
  (define-key markdown-mode-map [M-right] nil)
  (define-key markdown-mode-map [S-M-left] nil)
  (define-key markdown-mode-map [S-M-right] nil)
)
(add-hook 'markdown-mode-hook  'my-markdown-mode-hook)

;; Never use tabs. Only spaces. And only 2 of them!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq css-indent-level 2)
(setq css-indent-offset 2)

(scroll-bar-mode t)
(tool-bar-mode -1)

;; Emacs native line numbers
(global-display-line-numbers-mode t)

;; Undo
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
    (progn
      (global-undo-tree-mode 1)
      (defalias 'redo 'undo-tree-redo)

      (global-set-key (kbd "s-z") 'undo)
      (global-set-key (kbd "s-Z") 'redo)))


;; web-mode, for mixed HTML files
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
    (progn
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-enable-auto-pairing t))
      (setq web-mode-enable-current-element-highlight t))


;; Get haml mode
(use-package haml-mode
  :ensure t)

;; Javascript mode
(use-package js2-mode
  :ensure t
  :init (setq js-indent-level 2)
  :config (add-hook 'js-mode-hook 'js2-minor-mode))

;; Coffeescript
(use-package coffee-mode
  :ensure t
  :config (custom-set-variables
           '(coffee-tab-width 2)))

(use-package projectile
  :ensure t
  :diminish projectile-mode "Ⓟ"
  :init (progn
          (setq projectile-enable-caching nil)
          (setq projectile-switch-project-action 'projectile-dired))
  :config (progn
            (projectile-global-mode)
            (add-hook 'projectile-mode-hook 'projectile-rails-on)
            (add-to-list 'projectile-globally-ignored-directories "log")
            (add-to-list 'projectile-globally-ignored-directories "tmp")
            (add-to-list 'projectile-globally-ignored-directories "dist")
            (add-to-list 'projectile-globally-ignored-directories "public/raml/scripts")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".full.js")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".min.js")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".min.css")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".raw")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".vdi")
            ;; Specific for Platform161 project
            (add-to-list 'projectile-globally-ignored-directories "import")
            (add-to-list 'projectile-globally-ignored-directories "export")
            ;; Specific for Platform161's new UI project
            (add-to-list 'projectile-globally-ignored-directories "node_modules")
            (add-to-list 'projectile-globally-ignored-directories "docTypeScript")
            ;; Specific for Evadium project
            (add-to-list 'projectile-globally-ignored-files "*full.js")
            (add-to-list 'projectile-globally-ignored-directories "javascripts/oat"))
  :bind (("C-c p s g" . 'projectile-grep)))

;; Disable backups. We use git
(setq make-backup-files nil)

;; flx && flx-ido
(use-package flx-ido
  :ensure t
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (flx-ido-mode 1)
            ;; disable ido faces to see flx highlights.
            (setq ido-enable-flex-matching t)
            (setq ido-use-faces nil)))

;; ido-vertical-mode To show completion list vertically
(use-package ido-vertical-mode
  :ensure t
  :config (progn
            (ido-vertical-mode 1)
            (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)))

;; Delete grep headers
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'delete-grep-header)

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Custom keybindings to my taste

;; Command-T -> Select from recent files in the project
;; As it is assigned to Textmate's "goto file" command, global-set-key doesn't work and we have to use define-key with remap
;; See https://www.masteringemacs.org/article/mastering-key-bindings-emacs
(define-key (current-global-map) [remap textmate-goto-file] 'projectile-switch-to-buffer)

;; Ctrl-Command-T -> Select file in project (used when the file isn't already in a buffer)
;; I don't know why, Emacs doesn't understand "C-s-t" and instead wants <C-s-268632084>
(global-set-key (kbd "<C-s-268632084>") 'projectile-find-file)

;; Use the MacOS color picker
(require 'color-picker)
;; Ctrl-Command-C -> Open the Color Picker and return a color in Hex
(require 'color-picker)
(global-set-key (kbd "C-s-c") 'color-picker)

;; Set cursor color to white
(setq default-frame-alist
  '((cursor-color . "#ffffff")))

;; Load hightlight indentation
(require 'highlight-indentation)

;; Intelligent tab complete
(require 'tab-complete)

;; Open files in dired mode using 'open'
;; See http://jblevins.org/log/dired-open
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

;; RVM
(use-package rvm
  :ensure t
  :defer t
  :config (rvm-use-default))

;; Set the correct Ruby when changing files
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

;; Needed for RSpec below
(use-package inf-ruby
  :ensure t
  :defer t)

;; RSpec Mode
(use-package rspec-mode
  :ensure t
  :defer t
  :config (add-hook 'ruby-mode-hook
                    (lambda () (rspec-mode))
                    (add-hook 'dired-mode-hook 'rspec-dired-mode)))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

;; Proper highlighting of .arb files
(add-to-list 'auto-mode-alist '("\\.arb\\'" . ruby-mode))


;; YAML mode
(use-package yaml-mode
  :ensure t
  :diminish)

;; TODO -> Still not working
;; Change background color of non-active windows/buffers
(use-package auto-dim-other-buffers
  :ensure t
  :defer t
  :config ((auto-dim-other-buffers-mode)
           (set-face-background 'auto-dim-other-buffers-face "#4E3301")))

;; Code folding for Ruby and custom keybindings
;; See https://coderwall.com/p/u-l0ra/ruby-code-folding-in-emacs
(add-hook 'ruby-mode-hook
  (lambda () (hs-minor-mode)))

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
    `(ruby-mode
      ,(rx (or "def" "class" "module" "do" "{" "[")) ; Block start
      ,(rx (or "}" "]" "end"))                       ; Block end
      ,(rx (or "#" "=begin"))                        ; Comment start
      ruby-forward-sexp nil)))

(global-set-key (kbd "C-c h") 'hs-hide-block)
(global-set-key (kbd "C-c s") 'hs-show-block)
(global-set-key (kbd "s-y") 'hs-toggle-hiding)

;; TODO: When we try Angular and TS
;; ;; Typescript

;; (setq exec-path (append exec-path '("/Users/jes/.nvm/versions/node/v9.2.0/bin")))

;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   ;; company is an optional dependency. You have to
;;   ;; install it separately via package-install
;;   ;; `M-x package-install [ret] company`
;;   (company-mode +1)
;;   )

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; ;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ;; Karma
;; (require 'karma)

;; ;; Angular2 goodies
;; (add-to-list 'load-path "~/.emacs.d/my-lisp")
;; (require 'jes-ng2)
;; (add-hook 'typescript-mode-hook
;;   (lambda () (global-set-key (kbd "s-M-y") 'jes-ng2-alternate)))
;; (add-hook 'web-mode-hook
;;   (lambda () (global-set-key (kbd "s-M-y") 'jes-ng2-alternate)))

;; Disable non-magit version control
(delete 'Git vc-handled-backends)

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("<C-s-M-268632071>" . 'mc/mark-next-like-this)  ;; For some reason, 'g' is not allowed here
         ("C-s-M-S-g" . 'mc/mark-previous-like-this)))

;; Folding of XML
;; See https://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

;; Fast tramp
;; Disabled for termux
(setq tramp-default-method "ssh")

;; Uncomment next line only for debugging Tramp issues
;; (setq tramp-verbose 6)

;; Needed to connect to Android/Termux
;; TODO: Uncomment after loading TRAM
;; (add-to-list 'tramp-connection-properties
;;              (list (regexp-quote "android") "remote-shell" "/data/data/com.termux/files/usr/bin/bash"))

;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (add-to-list 'tramp-remote-path "/data/data/com.termux/files/usr/bin:/data/data/com.termux/files/usr/bin/applets")
