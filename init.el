;; Keys
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)
(setq mac-command-modifier 'super)

(add-to-list 'load-path "~/.emacs.d/my-lisp")

(add-to-list 'load-path "~/.emacs.d/vendor/aquamacs")
(require 'emulate-mac-keyboard-mode)
(emulate-mac-spanish-keyboard-mode)

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (require 'command-keys-for-linux))))

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
(global-set-key (kbd "C-s-f") 'projectile-grep)                   ;; "C-Cmd-f" Projectile grep (nicer, shorter shortcut)

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
 '(custom-enabled-themes '(wombat))
 '(ns-alternate-modifier 'meta)
 '(org-agenda-files "~/.agenda_files")
 '(package-selected-packages
   '(robe tree-sitter-langs gptel vdiff ruby-test-mode browse-at-remote csv-mode prettier jest osm gnuplot rubocop terraform-mode jq-mode rbenv sonic-pi dockerfile-mode restclient rbs-mode graphviz-dot-mode minitest minitest-mode editorconfig htmlize slim-mode vterm groovy-mode crontab-mode yasnippet-snippets yasnippet tide markdown-mode ox-reveal yaml-mode inf-ruby auto-dim-other-buffers auto-dim-other-buffers-mode undo-tree multiple-cursors rspec-mode magit ido-vertical-mode flx-ido projectile coffee-mode js2-mode haml-mode web-mode exec-path-from-shell use-package))
 '(safe-local-variable-values
   '((eval prettier-mode t)
     (web-mode-markup-indent-offset . 4)
     (rspec-spec-command . "rspec -Ispec/app")))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vterm-color-blue ((t (:foreground "SlateBlue1")))))


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
  (setq org-html-html5-fancy t)
  (setq org-html-doctype "html5")
  (define-key org-mode-map [M-left] nil)
  (define-key org-mode-map [M-right] nil)
  (define-key org-mode-map [S-M-left] nil)
  (define-key org-mode-map [S-M-right] nil)
  (turn-on-auto-fill)
)
(add-hook 'org-mode-hook  'my-org-mode-hook)

;; The same for Markdown
(defun my-markdown-mode-hook ()
  "Hook for Markdown mode tweaks."
  (define-key markdown-mode-map [M-left] nil)
  (define-key markdown-mode-map [M-right] nil)
  (define-key markdown-mode-map [S-M-left] nil)
  (define-key markdown-mode-map [S-M-right] nil)
  (turn-on-auto-fill)
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

;; Show column number
(column-number-mode t)

;; Auto revert mode
(global-auto-revert-mode t)

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
         ("\\.mjml\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
    (progn
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-enable-auto-pairing t))
      (setq web-mode-enable-current-element-highlight t))

;; Read .editorconfig files
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Get haml mode
(use-package haml-mode
  :ensure t)

;; Get slim mode
(use-package slim-mode
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

;; Org-mode
(require 'org)

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . true)
    (ruby . t)
    (shell . t)
    (dot . t)
    (sql . t)
    (gnuplot . t)
    (sqlite . t)))

;; Org-reveal
(use-package ox-reveal
  :ensure t
  :config (setq org-reveal-root ".")) ;; We serve always from an adhoc server

;; Htmlize for syntax coloring in Org-reveal
(use-package htmlize
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (progn
          (setq markdown-command "multimarkdown")
          (setq markdown-max-image-size '(700 . 300)))
  )

;; Graphviz dot mode
(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;; GNUplot mode
(use-package gnuplot
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode "Ⓟ"
  :init (progn
          (setq projectile-enable-caching nil)
          (setq projectile-switch-project-action 'projectile-dired))
  :config (progn
            (projectile-global-mode)
            (add-hook 'projectile-mode-hook 'projectile-rails-on)
            (add-to-list 'projectile-globally-ignored-directories "*log")
            (add-to-list 'projectile-globally-ignored-directories "*tmp")
            (add-to-list 'projectile-globally-ignored-directories "*dist")
            (add-to-list 'projectile-globally-ignored-directories "public/raml/scripts")
            (add-to-list 'projectile-globally-ignored-directories "bundle")
            (add-to-list 'projectile-globally-ignored-directories "vcr_cassettes")
            (add-to-list 'projectile-globally-ignored-directories "coverage")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".full.js")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".min.js")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".min.css")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".raw")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".vdi")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".png")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".mp4")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".enc")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".otf")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".xls")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".xlsx")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".doc")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".docx")
            ;; Specific for Platform161 project
            (add-to-list 'projectile-globally-ignored-directories "import")
            (add-to-list 'projectile-globally-ignored-directories "export")
            (add-to-list 'projectile-globally-ignored-directories "docker/resources/sql")
            ;; Specific for Platform161's new UI project
            (add-to-list 'projectile-globally-ignored-directories "*node_modules")
            (add-to-list 'projectile-globally-ignored-directories "docTypeScript")
            ;; Specific for Platform161's Jira Reports project
            (add-to-list 'projectile-globally-ignored-directories "output")

            ;; Specific for Lingokids' project
            (add-to-list 'projectile-globally-ignored-file-suffixes "tsbuildinfo")
            (add-to-list 'projectile-globally-ignored-files "build.gradle")

            ;; Specific for Marketer' project
            (add-to-list 'projectile-globally-ignored-file-suffixes ".svg")
            (add-to-list 'projectile-globally-ignored-file-suffixes ".po")
            (add-to-list 'projectile-globally-ignored-files "*map.js")
            (add-to-list 'projectile-globally-ignored-directories "app/views/banners")
            (add-to-list 'projectile-globally-ignored-directories "spec/fixtures/vcr_cassettes")
            (add-to-list 'projectile-globally-ignored-directories "storage")
            (add-to-list 'projectile-globally-ignored-directories "build")
            (add-to-list 'projectile-globally-ignored-directories "src/locales")
            (add-to-list 'projectile-globally-ignored-directories "public/packs")
            (add-to-list 'projectile-globally-ignored-directories "public/packs-test")
            (add-to-list 'projectile-globally-ignored-directories "public/uploads")

            ;; Specific for Buvagenda' project
            (add-to-list 'projectile-globally-ignored-directories "app/assets/builds")

            ;; Specific for Evadium project
            (add-to-list 'projectile-globally-ignored-files "*full.js")
            (add-to-list 'projectile-globally-ignored-directories "javascripts/oat"))
  :bind (("C-c p s g" . 'projectile-grep)
         ("M-s-s" . 'vterm-other-window)))

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

;; ruby-test-mode
(use-package ruby-test-mode
  :ensure t)

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
(global-set-key (kbd "C-s-t") 'projectile-find-file)

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

;; Make other window default copy target in dired
(setq dired-dwim-target t)

;; RBEnv
(use-package rbenv
  :ensure t
  :defer t)


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

;; Rubocop mode
(use-package rubocop
  :ensure t
  :defer t
  :config (progn
            (add-hook 'ruby-mode-hook 'rubocop-mode)
            (setq rubocop-check-command "rubocop --parallel --format emacs")))

;; ;; Rubocop mode
;; (use-package rubocop
;;   :ensure t
;;   :defer t
;;   :config (add-hook 'ruby-mode-hook 'rubocop-mode))

;; Proper highlighting of .arb files
(add-to-list 'auto-mode-alist '("\\.arb\\'" . ruby-mode))

;; Proper highlighting of .env and .envrc files
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.env\\.\\(development\\|test\\|production\\|local\\)\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.env\\.\\(development\\|test\\|production\\|local\\).\\(development\\|test\\|production\\)\\'" . sh-mode))

;; minitest mode
(use-package minitest
  :ensure t
  :defer t)

;; RBS mode
(use-package rbs-mode
  :ensure t)

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

;; Typescript

;; Plain .ts support
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook
  (lambda () (setq-default typescript-indent-level 2)))

(use-package tide
  :ensure t
  :defer t)

(setq exec-path (append exec-path '("/Users/jes/.nvm/versions/node/v9.2.0/bin")))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ;; (company-mode +1)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-next-checker 'typescript-tide 'typescript-tslint)
  (flycheck-add-next-checker 'tsx-tide 'typescript-tslint))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook #'setup-tide-mode)

;; Jest
(use-package jest
  :ensure t
  :defer t
  :after (web-mode)
  :hook (web-mode . jest-minor-mode)
  :bind (("C-c , m" . 'jest-file)
         ("C-c , s" . 'jest-function)
         ("C-c , p" . 'jest)))

;; Karma
(use-package karma
  :defer t)

;; Angular2 goodies
(add-to-list 'load-path "~/.emacs.d/my-lisp")
(require 'jes-ng2)
(add-hook 'typescript-mode-hook
  (lambda () (global-set-key (kbd "M-s-y") 'jes-ng2-alternate)))
(add-hook 'web-mode-hook
  (lambda () (global-set-key (kbd "M-s-y") 'jes-ng2-alternate)))

;; TSX support
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; ;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

;; (flycheck-add-next-checker 'typescript-tide 'typescript-tslint)
;; (flycheck-add-next-checker 'tsx-tide 'typescript-tslint)

;; enable eslint checker
;;(flycheck-add-mode 'javascript-eslint 'web-mode)


;; Groovy
(use-package groovy-mode
  :ensure t
  :defer t)
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

;; Disable non-magit version control
(delete 'Git vc-handled-backends)

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-s-M-g" . 'mc/mark-next-like-this)
         ("C-s-M-S-g" . 'mc/unmark-next-like-this)
         ("C-s-M-s" . 'mc/skip-to-next-like-this)
         ("C-s-M-a" . 'mc/mark-all-like-this)))

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
(require 'tramp)

(add-to-list 'tramp-connection-properties
             (list (regexp-quote "android") "remote-shell" "/data/data/com.termux/files/usr/bin/bash"))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/data/data/com.termux/files/usr/bin:/data/data/com.termux/files/usr/bin/applets")

;; Yasnippets
(use-package yasnippet
  :ensure t)
(use-package yasnippet-snippets
  :ensure t)
(yas-global-mode 1)

;; vterm
(use-package vterm
  :ensure t)

(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map (kbd "s-v") 'vterm-yank)
            (display-line-numbers-mode -1)))

;; restclient
(use-package restclient
  :ensure t
  :defer t)
(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

;; restclient-jq provides useful functions, like jq-set-var
;; restclient-jq is not included in the Melpa package. This seems to be a bug
;; But the code in restclient tries to load it, if present
;; So, to make it work until it's fixed, we just downloaded the file manually from GitHub
;; And added it to the load-path
;;
;; One last caveat: It only works after loading jq-mode, so you need to manually
;; enable jq-mode in a buffer before the first time you need jq-set-var to work
(add-to-list 'load-path "~/.emacs.d/vendor/restclient-jq.el")

;; jq-mode
(use-package jq-mode
  :ensure t
  :defer t)
(add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode))


;; Dockerfile
(add-to-list 'load-path "~/.emacs.d/vendor/dockerfile-mode.el")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Terraform
(use-package terraform-mode
  :ensure t
  :defer t)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))

;; OpenStreetMaps
(use-package osm
  :bind (("C-c m h" . osm-home)
         ("C-c m s" . osm-search)
         ("C-c m v" . osm-server)
         ("C-c m t" . osm-goto)
         ("C-c m x" . osm-gpx-show)
         ("C-c m j" . osm-bookmark-jump))

  :custom
  ;; Take a look at the customization group `osm' for more options.
  (osm-server 'default) ;; Configure the tile server
  (osm-copyright t))     ;; Display the copyright information

;; Prettier (facepalm!)
(use-package prettier
  :ensure t
  :defer t)

;; Use it only for Marketer JS projects
(dir-locals-set-class-variables 'prettier-js
                                '((typescript-mode . ((eval . (prettier-mode t))))
                                  (web-mode . ((eval . (prettier-mode t))))))

(dir-locals-set-directory-class "/Users/jes/Code/Marketer/marketer-frontend" 'prettier-js)
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/ui-library" 'prettier-js)
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/standalone-checkout" 'prettier-js)
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/iframe-modules-wrapper" 'prettier-js)



;; CSV mode
(use-package csv-mode
  :ensure t
  :defer t)
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

;; browse-at-remote
(use-package browse-at-remote
  :ensure t
  :init
    (progn (global-set-key (kbd "C-c g g") 'browse-at-remote)))

;; vdiff
(use-package vdiff
  :ensure t)


;; Sonic Pi

(defun sonic-pi-play ()
  (interactive)
  (shell-command "sendkeys -a 'Sonic Pi' -c '<c:r:command>'"))

(global-set-key (kbd "C-c C-p") 'sonic-pi-play)

;; Decent maximization
(setq frame-resize-pixelwise t)

;; ChatGPT
(setq gptel-default-mode 'org-mode)
(setq gptel-prompt-string "* ")

(use-package gptel
  :ensure t
  :init
    (progn (global-set-key (kbd "C-c g p t") 'gptel)))

(require 'macos-keychain)

(setq gptel-api-key (get-password-from-keychain "openai-api-key"))

;; Random keybindings
(global-set-key (kbd "C-c j") 'json-pretty-print)

;; Start server
(server-start)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Control vertical window splitting
(setq split-height-threshold nil)

;; For tree-sitter in Emacs 29
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; Robe
(use-package robe
  :ensure t)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-ts-mode-hook 'robe-mode)

(defun ruby-mode-variables () nil)
