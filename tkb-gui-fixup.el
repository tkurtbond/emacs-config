;;; See: 
;;; Fontname syntax: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#fontname_syntax
;;; POINT_SIZE: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes
;;; **NOT** PIXEL_SIZE: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#pixel_size
;; ALSO: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes
;;
;; Under X using POINT_SIZE in the font definition gets converted to
;; PIXEL_SIZE in emacs 'font frame parameter, but the original is
;; still in the 'font-parameter frame parameter.
;;
;; Oh.  You really only need to say "Fontname-10" or just "Fontname".  Duh.

(defun tkb-make-mono-font-xlfd (font-name point-size)
  (concat "-*-" font-name "-regular-normal-normal-*-*-"
                      (number-to-string (* 10 point-size)) ; XLFD POINT_SIZE
                      "-*-*-m-0-iso10646-1"))

(defun tkb-gui-fixup (&optional font-name point-size width height left top)
  "Fix up the emacs frames to have the right position and font."
  (interactive)

  (unless font-name  (setq font-name  (tkb-select-mono-font tkb-default-font-name)))
  (unless point-size (setq point-size (read-number "Font Size? " tkb-default-point-size)))
  (unless width      (setq width      (read-number "Width? "
                                                   tkb-default-width)))
  (unless height     (setq height     (read-number "Height? "
                                                   tkb-default-height)))
  (unless left       (setq left       (read-number "Left? " tkb-default-left)))
  (unless top        (setq top        (read-number "Top? " tkb-default-top)))

  (setq tkb-default-font-name  font-name)
  (setq tkb-default-point-size point-size)
  (setq tkb-default-font       (tkb-mf point-size font-name))
  (setq tkb-default-width      width)
  (setq tkb-default-height     height)
  (setq tkb-default-left       left)
  (setq tkb-default-top        top)
  (setq tkb-default-description "Set by tkb-gui-fixup")

  (setf (cdr (assq 'font   default-frame-alist)) tkb-default-font)
  (setf (cdr (assq 'width  default-frame-alist)) width)
  (setf (cdr (assq 'height default-frame-alist)) height)
  (setf (cdr (assq 'left   default-frame-alist)) left)
  (setf (cdr (assq 'top    default-frame-alist)) top)

  (setf (cdr (assq 'font   initial-frame-alist)) tkb-default-font)
  (setf (cdr (assq 'width  initial-frame-alist)) width)
  (setf (cdr (assq 'height initial-frame-alist)) height)
  (setf (cdr (assq 'left   initial-frame-alist)) left)
  (setf (cdr (assq 'top    initial-frame-alist)) top)
  
  (cl-loop for f in (frame-list) do
        (set-frame-parameter f 'font   tkb-default-font)
        (set-frame-parameter f 'width  width)
        (set-frame-parameter f 'height height)
        (set-frame-parameter f 'left   left)
        (set-frame-parameter f 'top    top)))

(defun tkb-urlich ()
  (interactive)
  (tkb-gui-fixup 14 80 70))
(defun tkb-helblindi ()
  (interactive)
  (tkb-gui-fixup 10 80 55))
;; (tkb-gui-fixup 8 80 55)



