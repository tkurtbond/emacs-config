;;; See: 
;;; Fontname syntax: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#fontname_syntax
;;; POINT_SIZE: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes
;;; **NOT** PIXEL_SIZE: https://www.x.org/releases/X11R7.6/doc/xorg-docs/specs/XLFD/xlfd.html#pixel_size
;; ALSO: https://wiki.archlinux.org/title/X_Logical_Font_Description#Font_sizes

;; Under X using POINT_SIZE in the font definition gets converted to
;; PIXEL_SIZE in emacs 'font frame parameter, but the original is
;; still in the 'font-parameter frame parameter.

(defun tkb-make-mono-font-xlfd (font-name point-size)
  (concat "-*-" font-name "-regular-normal-normal-*-*-"
                      (number-to-string (* 10 point-size)) ; XLFD POINT_SIZE
                      "-*-*-m-0-iso10646-1"))

(defun tkb-gui-fixup (&optional point-size width height left top)
  "Fix up the emacs frames to have the right position and font."
  (interactive)

  (unless point-size (setq point-size (read-number "Font Size? " 10)))
  (unless width      (setq width      (read-number "Width? "
                                                   tkb-default-width)))
  (unless height     (setq height     (read-number "Height? "
                                                   tkb-default-height)))
  (unless left       (setq left       (read-number "Left? " tkb-default-left)))
  (unless top        (setq top        (read-number "Top? " tkb-default-top)))

  (let ((font
         ;;(tkb-make-mono-font-xlfd "Go Mono" 10)
         (tkb-make-mono-font-xlfd "Ubunto Mono" 10)))
    (cl-loop for f in (frame-list) do
         (set-frame-parameter f 'font   font)
         (set-frame-parameter f 'width  width)
         (set-frame-parameter f 'height height)
         (set-frame-parameter f 'left   left)
         (set-frame-parameter f 'top    top)
         (setf (cdr (assq 'font   default-frame-alist)) font)
         (setf (cdr (assq 'width  default-frame-alist)) width)
         (setf (cdr (assq 'height default-frame-alist)) height)
         (setf (cdr (assq 'left   default-frame-alist)) left)
         (setf (cdr (assq 'top    default-frame-alist)) top))))

(defun tkb-urlich ()
  (interactive)
  (tkb-gui-fixup 14 80 70))
(defun tkb-helblindi ()
  (interactive)
  (tkb-gui-fixup 10 80 55))
;; (tkb-gui-fixup 8 80 55)


(defun tkb-list-mono-fonts ()
  (interactive)
  (seq-filter (lambda (font)
              (when-let ((info (font-info font)))
                (string-match-p "spacing=100" (aref info 1))))
            (font-family-list)))
