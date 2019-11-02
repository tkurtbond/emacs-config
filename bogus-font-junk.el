(defun x ()
  (interactive)
  (let ((b (point-min))
	(e (point-max)))
    (write-region b e "c:/HP-CLJ-4650DN-PCL-6" t 0)))

(setq full-font-name 
      "-outline-DejaVu Sans Mono-normal-r-normal-normal-16-120-96-96-c-*-iso8859-1")
(setq font-name
      "-outline-DejaVu Sans Mono-normal-r-normal-normal-*-*-*-*-c-*-iso10646-1")
(setq font-vec
      ["outline" "DejaVu Sans Mono-normal" "r" "normal" "normal" "*" "*" "*" "*" "c" "*" "iso10646-1"])
(font-info full-font-name)
(font-info font-vec)
(x-resolve-font-name "-*-DejaVu Sans Mono-normal-r-normal-normal-*-*-*-*-c-*-iso10646-1")

(x-must-resolve-font-name ["*" "DejaVu Sans Mono-normal" "r" "normal" "normal" "*" "*" "*" "*" "c" "*" "iso10646-1"])

frame-char-height

(pp (x-list-fonts "*DejaVu Sans Mono*")); less useful than I thought.

(x-family-fonts "DejaVu Sans Mono")

(insert (prin1-to-string (x-list-fonts "*")))

(set-frame-font (car (x-list-fonts "-outline-DejaVu Sans Mono-normal-r-normal-normal-*-120-*-*-c-*-iso10646-1")))


(setq xfont "-*-dejavu sans mono-medium-r-normal-*-*-*-*-*-*-*-iso10646-1")
(x-list-fonts xfont)
(set-frame-font (x-resolve-font-name xfont))
(defun x ()
  (interactive)
  (let* ((font (x-resolve-font-name xfont))
	 (_ (set-frame-font font))
	 (fi (font-info font))
	 (fh (aref fi 3))
	 (dph (display-pixel-height))
	 (sh (/ (- dph 10 120) fh)))
    (message "fh: %d dph: %d sh: %d font: %s fi: %S" fh dph sh font fi)
    (set-frame-font font)
    (set-frame-height nil sh)))

;; 72 pt = 1 inch = 25.4 mm
  
