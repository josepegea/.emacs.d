;; Refresh global menu on change
;; See https://stackoverflow.com/questions/7497440/how-to-enable-global-menu-bar-for-emacs-on-ubuntu-unity-11-04/7499532#7499532

(add-hook 'menu-bar-update-hook (lambda () (interactive) (progn (menu-bar-mode -1) (menu-bar-mode 1))))

(provide 'refresh-global-menu-bar)
