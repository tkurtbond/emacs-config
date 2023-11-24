;;; See: 
;;; Fontname syntax: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#fontname_syntax
;;; POINT_SIZE: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes
;;; **NOT** PIXEL_SIZE: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#pixel_size
;; ALSO: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes

(defun tkb-gui-fixup (prefix)
  "Fix up the emacs frames to have the right position and font."
  (interactive "P")
  (let ((point-size 10)
        (height 55)
        (width 80))
    (when prefix
      (setq point-size (read-number "Font Size?" point-size))
      (setq height (read-number "Height?" height))
      (setq width (read-number "Width?" width))
      (setq font (concat "-*-Go Mono-regular-normal-normal-*-*-"
                         (number-to-string (* 10 point-size)) ; XLFD POINT_SIZE
                            "-*-*-m-0-iso10646-1")))
      
    (cl-loop for f in (frame-list) do
         (set-frame-parameter f 'font font)
         (set-frame-parameter f 'height height)
         (set-frame-parameter f 'width width)
         (setf (cdr (assq 'font default-frame-alist)) font)
         (setf (cdr (assq 'height default-frame-alist)) height)
         (setf (cdr (assq 'width default-frame-alist)) width)
         )))
