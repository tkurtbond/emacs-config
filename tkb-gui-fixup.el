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

  (unless font-name  (setq font-name  (tkb-select-mono-font tkb-default-font)))
  (unless point-size (setq point-size (read-number "Font Size? " 10)))
  (unless width      (setq width      (read-number "Width? "
                                                   tkb-default-width)))
  (unless height     (setq height     (read-number "Height? "
                                                   tkb-default-height)))
  (unless left       (setq left       (read-number "Left? " tkb-default-left)))
  (unless top        (setq top        (read-number "Top? " tkb-default-top)))

  (let ((font (format "%s-%d" font-name point-size)))
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


;; See: https://emacs.stackexchange.com/a/50215

(defun tkb-mono-fonts ()
  (sort (seq-uniq (seq-filter (lambda (font)
                                (when-let ((info (font-info font)))
                                  (and (string-match-p "spacing=100" (aref info 1))
                                       (not (or (string-match-p "Noto Color Emoji" (aref info 1))
                                                (string-match-p "Noto Sans SignWriting" (aref info 1)))))))
                              (font-family-list))
                  #'string=)
        #'string<))

(defun tkb-list-mono-fonts ()
  (interactive)
  (let ((buf (get-buffer-create "*Mono Fonts*"))
        (font-names (tkb-mono-fonts)))
    (with-output-to-temp-buffer buf
      (cl-loop for font-name in font-names
         do (progn (princ font-name) (terpri))))))

(defun tkb-display-mono-fonts ()
  (interactive)
  (loop for font-name in (tkb-mono-fonts)
     do (progn
          (set-frame-font font-name)
          (unless (y-or-n-p (format "Font is %s.  Continue? " font-name))
            (cl-return)))))

(defun tkb-select-mono-font (&optional default)
  (interactive)
  (let ((fonts (tkb-mono-fonts)))
    (completing-read "Monospace Font? " fonts)))

(defun tkb-select-frame-font ()
  (interactive)
  (let ((font (tkb-select-mono-font)))
    (if (not (string-blank-p font))
        (set-frame-font font)
      (message "No font selected!"))))
