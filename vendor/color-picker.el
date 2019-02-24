;; Use the Mac System Color Panel to capture hex color values in any mode
;; Taken from https://gist.github.com/a3ammar/2357d6115ddd999c23e6
;; And https://www.reddit.com/r/emacs/comments/2hscnp/make_emacs_use_os_x_color_picker/
;;
;; ToDo: Try to use Aquamacs' own Color Picker instead of calling Finder
;;

(defun nscolor2hex (color)
  "Converts colors from `NSColor' format to hex values"
  (concat "#"                           ; Add # at the front
          (mapconcat 'identity          ; concate the list
                     (mapcar '(lambda (x) ;returns ("hex" "hex" "hex")
                                (let ((col (lsh (string-to-int x) -8)))
                                  (if (< col 16)
                                      (format "0%x" col)
                                    (format "%x" col))))
                             (split-string (s-replace "\"" "" color) ",")) "")))

(defun color-picker (&optional list buffer-name callback)
  "Calls OS X color picker and insert the chosen color. It is really messy because of applyscript"
  (interactive)
  (let ((result
         (do-applescript "tell application \"Finder\"
	activate
set result to \"\"
set x to (choose color)
set result to item 1 of x as string
set result to result & \",\"
set result to result & item 2 of x as string
set result to result & \",\"
set result to result & item 3 of x as string
return result
end tell")))
    (if callback ; For Easy Customization
        (funcall callback (nscolor2hex result))
      (insert (nscolor2hex result)))
    (do-applescript "tell application \"Emacs\" to activate")))

(provide 'color-picker)
