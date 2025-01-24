;; Refresh global menu on change
;; See https://stackoverflow.com/questions/7497440/how-to-enable-global-menu-bar-for-emacs-on-ubuntu-unity-11-04/7499532#7499532

(defun menuupdate ()
  "Updates the menu bar"
  (interactive)
  (menu-bar-mode -1) (menu-bar-mode 1))

(global-set-key (kbd "C-s-M-M") 'menuupdate)

(provide 'refresh-global-menu-bar)
