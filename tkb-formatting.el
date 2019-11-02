;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-formatting.el -- Formating options: faces, fonts, character sets, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-formatting.el 1.2 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Display; see Emacs > Display > European Display
(when nil
  ;; Not with LANG=en_US.UTF-8
  (cond ((<= (string-to-number (car (split-string emacs-version "\\."))) 21)
	 (standard-display-european 1)
	 (unless tkb-xemacs-p
	   (set-language-environment "Latin-1")))
	(t
	 )))


;;; Faces
;; When I don't actually have a bold italic...
(unless (face-font 'bold-italic)
  (set-face-foreground 'bold-italic "red"))
;; ...and when I don't actually have an italic.
(unless (face-font 'italic)
  (set-face-foreground 'italic "blue"))

;; Or should I use this?
(defun tkb-modify-faces ()
  (interactive)
  (when (or (not (face-font 'bold-italic))
         (not (string-match "^[oi]"
                      (aref (x-decompose-font-name
                          (face-font 'bold-italic)) 3))))
    (set-face-underline-p 'bold-italic t))
  (when (or (not (face-font 'italic))
         (not (string-match
            "^[oi]"
            (aref (x-decompose-font-name (face-font 'italic)) 3))))
    (set-face-underline-p 'italic t)))



;(set-face-background 'bold "white")


(make-face 'doc-face)
(set-face-foreground 'doc-face "darkgreen")
(make-face 'string-face)
(set-face-foreground 'string-face "blue")
(copy-face 'italic 'comment-face)
(set-face-foreground 'comment-face "darkorchid")
;; yellowgreen, olivedrab, darkkhaki, goldenrod, darkgoldenrod, rosybrown,
;; indianred, saddlebrown(too close to black), sienna, peru, sandybrown,
;; chocolate, firebrick, orange, coral, hotpink, deeppink, maroon, violetred,
;; magenta, darkorchid, blueviolet, or scheme, lightsteelblue3, darkgoldenrod3,
;; purple

(when (or window-system tkb-xemacs-p (and (boundp 'emacs-major-version)
					  (>= emacs-major-version 21)))
  (load-library "font-lock")
  (cond (tkb-xemacs-p
	 (font-lock-use-default-maximal-decoration))
	(t
	 (setq font-lock-maximum-decoration t)
	 (global-font-lock-mode 1))))

;; Turn off multi-byte characters.
; (setq-default enable-multibyte-characters nil)

;;; end of tkb-formatting.el
