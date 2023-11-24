;;; See: 
;;; Fontname syntax: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#fontname_syntax
;;; POINT_SIZE: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes
;;; **NOT** PIXEL_SIZE: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#pixel_size
;; ALSO: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes

(defun tkb-gui-prompt-fixup (prefix)
  "Fix up the emacs frames to have the right position and font."
  (interactive "P")
  (let ((point-size 10)
        (width 80)
        (height 55))
    (when prefix
      (setq point-size (read-number "Font Size?" point-size))
      (setq width (read-number "Width?" width))
      (setq height (read-number "Height?" height)))
    (tkb-gui-fixup point-size width height)))


(defun tkb-gui-fixup (point-size width height)
  "Fix up the emacs frames to have the right position and font."
  (let ((font (concat "-*-Go Mono-regular-normal-normal-*-*-"
                      (number-to-string (* 10 point-size)) ; XLFD POINT_SIZE
                      "-*-*-m-0-iso10646-1")))
    (cl-loop for f in (frame-list) do
         (set-frame-parameter f 'font font)
         (set-frame-parameter f 'width width)
         (set-frame-parameter f 'height height)
         (setf (cdr (assq 'font default-frame-alist)) font)
         (setf (cdr (assq 'width default-frame-alist)) width)
         (setf (cdr (assq 'height default-frame-alist)) height))))
;; (tkb-gui-fixup 10 80 55)
;; (tkb-gui-fixup 8 80 55)
