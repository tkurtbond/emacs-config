(defun tkb-w32-initial-font-and-size-orig (prefix)
  (interactive "P")
  (require 'cl) ;; Because I sometimes use this after emacs -q, and it needs cl.
  (when nil 
    (concat "-outline-Bitstream Vera Sans Mono-normal-"
	    "r-normal-normal-11-110-96-96-c-*-iso10646-1")
    "-outline-Courier New-normal-r-normal-normal-11-82-96-96-c-*-iso10646-1"
    "-outline-Courier New-normal-r-normal-normal-11-82-96-96-c-*-iso10646-1")

  ;; Ok if don't need unicode:
  ;; "-raster-Dina-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1"
  ;; "-outline-DejaVu Sans Mono-normal-r-normal-normal-12-90-96-96-c-*-iso10646-1"

  ;; need to do something with display-pixel-height
  (multiple-value-setq
      (tkb-default-font tkb-default-height)
    (macrolet ((vdef (font height)
		     `(values ,font (if (and (boundp 'tkb-default-height)
					     prefix)
					(if (symbolp prefix)
					    tkb-default-height
					  prefix)
				      ,height))))
      (case 'unifont			; 'dejavu-13pt
	(courier-11pt			;; w32 says this is 11pt
	 (vdef
	  "-outline-Courier New-normal-r-normal-normal-15-*-*-*-c-*-iso10646-1"
	  55))
	(dejavu-9pt ;; w32 says this is 9pt
	 (vdef 
	  "-outline-DejaVu Sans Mono-normal-r-normal-normal-12-90-96-96-c-*-iso10646-1"
	  55))
	(dejavu-10pt ;; w32 says this is 10pt
	 (vdef 
	  "-outline-DejaVu Sans Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1"
	  55))
	(dejavu-11pt ;; w32 says this is 11pt
	 (vdef
	  "-outline-DejaVu Sans Mono-normal-r-normal-normal-15-112-96-96-c-*-iso10646-1"
	  55))
	(dejavu-13pt ;; w32 says this is 13 point; is it iso10646-1 by default?
	 (vdef 
	  "-outline-DejaVu Sans Mono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
	  50))
	;; ftp://ftp.gnu.org/pub/gnu/freefont/
	;; http://www.gnu.org/software/freefont/
	(freefont-14pt ;; w32 says this is 14 point; wider than dejavu 13pt
	 (vdef
	  "-outline-FreeMono-normal-r-normal-normal-19-142-96-96-c-*-iso10646-1"
	  50))
	(freefont-13pt ;; w32 says this is 13 point
	 (vdef
	  "-outline-FreeMono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
	  60))
	(unifont
	 ;; w32 doesn't show this in font selection dialog...
	 ;; Windows Font Explorer shows 12 pt as best looking size.
	 (vdef
	  "-outline-unifont-medium-r-normal-normal-*-*-96-96-p-*-iso10646-1"
	  60))
	)))
  (setq tkb-default-top 20)

  (set-frame-font tkb-default-font)
  (let* ((dh (display-pixel-height))
	 (ch (frame-char-height))
	 (nfh (truncate (- (/ dh ch) (* (/ tkb-default-top ch) 2) (* (/ dh ch) .10)))))
    (when nil (not (y-or-n-p (format "dh: %d ch: %d nfh: %d " dh ch nfh)))
	  (error "not doing it"))
    ;;  FIXME: make this work if executed multiple times.
    ;;(set-frame-parameter nil 'height nfh)
    (set-frame-height nil nfh)
    (setq default-frame-alist
	  (append default-frame-alist
		  `((width . 80)
		    ;;(height . ,tkb-default-height)
		    (height . ,nfh)
		    (top . ,tkb-default-top)
		    (left . (- 50))   ;2008-07-13: -20 broke with 22.2
		    (font . ,tkb-default-font)
		    (background-color . "wheat")
		    (foreground-color . "black"))))
    (setq initial-frame-alist default-frame-alist)
    (tool-bar-mode -1)
    ;;(set-frame-parameter nil 'height tkb-default-height)
    (set-frame-parameter nil 'font tkb-default-font)
    (set-frame-parameter nil 'top tkb-default-top)))
