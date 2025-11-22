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
    (require 'command-keys-for-linux)
    (require 'refresh-global-menu-bar))))

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
 '(auth-source-save-behavior nil)
 '(coffee-tab-width 2)
 '(cua-normal-cursor-color "#656565")
 '(cursor-type '(bar . 2))
 '(custom-enabled-themes '(wombat))
 '(custom-safe-themes
   '("77f1e155387d355fbbb3b382a28da41cc709b2a1cc71e7ede03ee5c1859468d2"
     default))
 '(debug-on-error nil)
 '(ns-alternate-modifier 'meta)
 '(org-agenda-files "~/.agenda_files")
 '(package-selected-packages
   '(adoc-mode aider auto-dim-other-buffers browse-at-remote bundler
               coffee-mode company consult csv-mode dirvish eglot
               embark embark-consult exec-path-from-shell flx-ido
               gnuplot gptel graphviz-dot-mode groovy-mode haml-mode
               htmlize ido-vertical-mode jest jq-mode kuronami-theme
               marginalia minitest multiple-cursors orderless
               ox-reveal peg pg pgmacs prettier prettier-rc rbenv
               rbs-mode restclient robe rspec-mode rubocop
               ruby-test-mode slim-mode terraform-mode tide
               tree-sitter-langs undo-tree vc-use-package vdiff
               vertico vterm web-mode yaml-mode yasnippet-snippets))
 '(package-vc-selected-packages
   '((pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs")
     (pg :vc-backend Git :url "https://github.com/emarsden/pg-el")
     (vc-use-package :vc-backend Git :url
                     "https://github.com/slotThe/vc-use-package")))
 '(prettier-rc-use-editorconfig nil)
 '(prettier-rc-use-node-modules-bin t)
 '(prettier-rc-use-package-json nil)
 '(safe-local-variable-directories '("/home/jes/Code/Marketer/marketer-frontend/"))
 '(safe-local-variable-values
   '((eval prettier-rc-mode t) (web-mode-markup-indent-offset . 4)
     (rspec-spec-command . "rspec -Ispec/app")))
 '(vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Hg))
 '(warning-suppress-types '((comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#181a26" :foreground "#c9c9c9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 103 :width normal :foundry "JB" :family "JetBrains Mono"))))
 '(cursor ((t (:background "white smoke"))))
 '(dirvish-hl-line ((t (:inherit highlight :extend t))))
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
      (setq undo-tree-auto-save-history nil)

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
      (setq web-mode-enable-auto-pairing t)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-auto-indentation nil)))

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

(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "sqlite"))))

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

            ;; Specific for SmartSale' project
            (add-to-list 'projectile-globally-ignored-directories ".next")

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
;; (define-key (current-global-map) [remap textmate-goto-file] 'projectile-switch-to-buffer)
;; Not anymore. Now we use consult
(define-key (current-global-map) [remap textmate-goto-file] 'consult-project-buffer)

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

;; Open files in dired mode

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (require 'linux-open))))
(cond
 ((string-equal system-type "darwin")
  (progn
    (require 'macos-open))))

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

;; company-mode
(use-package company
  :ensure t
  :defer t)


;; Typescript

;; Plain .ts support
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-hook 'typescript-mode-hook
  (lambda () (setq-default typescript-indent-level 2)))

(use-package tide
  :ensure t
  :defer t)

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
  (company-mode +1)
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

(setq vterm-max-scrollback 100000)

(add-hook 'vterm-mode-hook
          (lambda ()
            (define-key vterm-mode-map (kbd "s-v") 'vterm-yank)
            (define-key vterm-mode-map (kbd "s-<return>") 'vterm-copy-mode)
            (display-line-numbers-mode -1)))

;; Useful for copy-mode in vterm while running Claude Code

(defvar my/vterm-cursor-type-backup nil)

(add-hook 'vterm-copy-mode-hook
          (lambda ()
            (setq my/vterm-cursor-type-backup cursor-type)
            (setq cursor-type 'box)))

(add-hook 'vterm-copy-mode-exit-hook
          (lambda ()
            (setq cursor-type my/vterm-cursor-type-backup)))

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

;; Prettier
;; Replaced prettier.el with prettier-rc.el
;; because the first one stopped honoring
;; local config for projects
(use-package prettier-rc
  :ensure t
  :defer t)

;; Use it only for Marketer JS projects
(dir-locals-set-class-variables 'prettier-js
                                '((typescript-mode . ((eval . (prettier-rc-mode t))))
                                  (web-mode . ((eval . (prettier-rc-mode t))))))
;; For macOS
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/marketer-frontend" 'prettier-js)
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/ui-library" 'prettier-js)
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/standalone-checkout" 'prettier-js)
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/iframe-modules-wrapper" 'prettier-js)
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/property-picker" 'prettier-js)
;; For Linux
(dir-locals-set-directory-class "/home/jes/Code/Marketer/marketer-frontend" 'prettier-js)
(dir-locals-set-directory-class "/home/jes/Code/Marketer/ui-library" 'prettier-js)
(dir-locals-set-directory-class "/home/jes/Code/Marketer/standalone-checkout" 'prettier-js)
(dir-locals-set-directory-class "/home/jes/Code/Marketer/iframe-modules-wrapper" 'prettier-js)
(dir-locals-set-directory-class "/home/jes/Code/Marketer/property-picker" 'prettier-js)



;; Configure a different rspec command for marketer project

;; If we don't do that Emacs warns for every file loaded in one of those dires
(put 'rspec-spec-command 'safe-local-variable #'stringp)

(dir-locals-set-class-variables 'marketer-backend
                                '((nil . ((rspec-spec-command . "bin/rspec")))))
;; For macOS
(dir-locals-set-directory-class "/Users/jes/Code/Marketer/marketer" 'marketer-backend)
;; For Linux
(dir-locals-set-directory-class "/home/jes/Code/Marketer/marketer" 'marketer-backend)


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
(setq gptel-model 'gpt-4.1)
(setq gptel-expert-commands t)

(use-package gptel
  :ensure t
  :pin melpa
  :init
    (progn
      (global-set-key (kbd "C-c g p t") 'gptel)
      (global-set-key (kbd "C-c g p s") 'gptel-send)))

(gptel-make-openai "llamafile"          ;Any name
  :stream t                             ;Stream responses
  :protocol "http"
  :host "localhost:8080"                ;Llama.cpp server location
  :models '("mixtral"))                    ;Any names, doesn't matter for Llama

(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (require 'gptel-key-linux))))

(cond
 ((string-equal system-type "darwin")
  (progn
    (require 'gptel-key-macos))))

(add-hook 'gptel-mode-hook
          (lambda ()
            (run-at-time
             "0.1 sec" nil
             (lambda ()
               (when (derived-mode-p 'org-mode)
                 (auto-fill-mode -1)
                 (visual-line-mode 1)
                 (when (fboundp 'visual-wrap-prefix-mode)
                   (visual-wrap-prefix-mode 1)))))))

;; Random keybindings
(global-set-key (kbd "C-c j") 'json-pretty-print)

;; Start server
(server-start)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Control vertical window splitting
(setq split-height-threshold nil)
(setq split-width-threshold 170)

;; For tree-sitter in Emacs 29
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

; Use eglot for Ruby
;; (add-hook 'ruby-mode-hook 'eglot)
;; (add-hook 'ruby-ts-mode-hook 'eglot)

;; (require 'ruby-copy-namespaced-class-name)
;; (global-set-key (kbd "C-c M-c") 'ruby-copy-namespaced-class-name)

;; AsciiDoc mode
(use-package adoc-mode
  :ensure t
  :defer t)
(add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

;; Bundler utils
(use-package bundler
  :ensure t
  :defer t)

;; Copilot

;; Copilot Dependencies
(use-package dash
  :ensure t
  :defer t)

;; Copilot package
(add-to-list 'load-path "/home/jes/.emacs.d/vendor/copilot/copilot.el")
(require 'copilot)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; PGemacs
(use-package pg
  :vc (:fetcher github :repo emarsden/pg-el))

(use-package pgmacs
  :vc (:fetcher github :repo emarsden/pgmacs))

;; ;; aider.el
;; (use-package aider
;;   :vc (:fetcher github :repo "tninja/aider.el")
;;   ;; :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;   :config
;;   ;; Use claude-3-5-sonnet cause it is best in aider benchmark
;;   (setq aider-args '("--model" "gpt-4o-mini"))
;;   (setenv "OPENAI_API_KEY" gptel-key-linux)
;;   (global-set-key (kbd "C-c a") 'aider-transient-menu))

;; Copied from rafadc :-D
;; Paste images into org-mode docs

(defun org-image-from-clipboard ()
  "Takes a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (let ((filename (concat
                  (make-temp-name
                   (concat (file-name-nondirectory (buffer-file-name)) "_imgs/" (format-time-string "%Y%m%d_%H%M%S_")) ) ".png")))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
    (call-process "pasteimg" nil nil nil filename)
    (if (file-exists-p filename)
        (insert (concat "[[file:" filename "]]")))
    (org-display-inline-images)))

(use-package aider
  :config
  ;; For latest claude sonnet model
  (setq aider-args '("--model" "gpt-4.1" "--no-auto-commits" "--no-auto-accept-architect"))
  (setenv "OPENAI_API_KEY" gptel-api-key)
  ;; Or gemini model
  ;; (setq aider-args '("--model" "gemini"))
  ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
  ;; Or chatgpt model
  ;; (setq aider-args '("--model" "o4-mini"))
  ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
  ;; Or use your personal config file
  ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))
  ;; ;;
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c a") 'aider-transient-menu)) ;; for wider screen
  ;; or use aider-transient-menu-2cols / aider-transient-menu-1col, for narrow screen

;; Use vterm for certain RSpec compilations
;; Use [C-u C-c , s] to execute the spec under current line on an vterm buffer
;; Kindly generated by ChatGPT after a long trying session

(defun my/project-root ()
  "Get project root for Rails/RSpec."
  (or (locate-dominating-file default-directory "spec")
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun my/rspec-current-example-command ()
  "Build RSpec command for current example."
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (root (my/project-root))
         (relpath (if (and root file)
                      (file-relative-name file root)
                    file)))
    (format "bundle exec rspec %s:%d" relpath line)))

(defun my/rspec-run-in-vterm (cmd)
  "Run CMD in a vterm buffer in the project root."
  (let* ((default-directory (my/project-root))
         (buffer-name "*RSpec-vterm*")
         (buf (get-buffer buffer-name)))
    (pop-to-buffer (or buf (vterm-other-window buffer-name)))
    (goto-char (point-max))
    (vterm-send-string cmd)
    (vterm-send-return)))

(defun my/rspec-runner (orig-fun &rest args)
  (if current-prefix-arg
      (let ((cmd (my/rspec-current-example-command)))
        (message "Sending to vterm: %S" cmd)
        (my/rspec-run-in-vterm cmd))
    (apply orig-fun args)))

(advice-add 'rspec-verify-single :around #'my/rspec-runner)

; Rails Web Components Toggle
(require 'rails-component-toggle)

; vterms per project
(require 'jes-project-vterm)

;; Dired and dirvish
;; See https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#sample-config

(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("c" "~/Code/"                     "Code")
     ("s" "~/Stuff/"                    "Stuff")))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(collapse file-modes file-time file-size)
        dirvish-side-attributes
        '(collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   ("M-<up>" . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

; Consult
;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-t" . consult-fd)                      ;; JES
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; Marginalia: Anotate completion options

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Embark: Contextual actions

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Open an vterm in the selected path
(defun my/embark-vterm-here (file)
  "Open a vterm buffer in the directory of FILE.
If FILE is a directory, open vterm there; if a file, open in its directory."
  (let ((dir (if (file-directory-p file)
                 file
               (file-name-directory file))))
    (require 'vterm)
    (let ((default-directory dir))
      (vterm (generate-new-buffer-name "*vterm*")))))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "V") #'my/embark-vterm-here))


;; Vertico, instead of ido

(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Orderless: better completion

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring
