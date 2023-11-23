;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tkb-gui-setup.el -- Setup things that only work under a GUI.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Starting tkb-gui-setup.el")

(setq tkb-fonts
      `(("courier-11pt" ;; w32 says this is 11pt
	 "-outline-Courier New-normal-r-normal-normal-15-*-*-*-c-*-iso10646-1"
	 55 "constant courier-11pt 55 high")
	("dejavu-9pt" ;; w32 says this is 9pt
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-12-90-96-96-c-*-iso10646-1"
	 55 "constant dejavu-9pt 55 high")
	("dejavu-10pt" ;; w32 says this is 10pt
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1"
	 55 "constant dejavu-10pt 55 high")
	("dejavu-11pt" ;; w32 says this is 11pt
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-15-112-96-96-c-*-iso10646-1"
	 55 "constant dejavu-10pt 55 high")
	("dejavu-13pt" ;; w32 says this is 13 point; is it iso10646-1 by default?
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
	 50 "constant dejavu-13pt 50 high")
	;; ftp://ftp.gnu.org/pub/gnu/freefont/
	;; http://www.gnu.org/software/freefont/
	("freefont-14pt" ;; w32 says this is 14 point; wider than dejavu 13pt
	 "-outline-FreeMono-normal-r-normal-normal-19-142-96-96-c-*-iso10646-1"
	 50 "constant freefont-14pt 50 high")
	("freefont-13pt" ;; w32 says this is 13 point
	 "-outline-FreeMono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
	 55 "constant freefont-13pt 55 high")
	("unifont"
	 ;; w32 doesn't show this in font selection dialog...
	 ;; Windows Font Explorer shows 12 pt as best looking size.
	 "-outline-unifont-medium-r-normal-normal-*-*-96-96-p-*-iso10646-1"
	 65 "constant unifont 65 high")

	;; Ok if don't need unicode:
	("dina" "-raster-Dina-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1" 55 "constant dina 55 high")
	("consolas" "-outline-Consolas-normal-r-normal-normal-17-127-96-96-c-*-iso8859-1" 55 "constant consolas 55 high")
	("inconsolata" "-outline-Inconsolata-medium-r-normal-normal-17-127-96-96-c-*-iso8859-1" 55 "constant inconsolata 55 high")

	("Go Mono" ,@(cond
                       ;; Maybe I should do something with
                       ;; (assoc 'mm-size (frame-monitor-attributes)) ?
                       ((>= (display-pixel-height) 2280)
                        ;; Retina display probably, so use smaller font
                        '("-*-Go Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1" 56 "go display-pixel-height >= 2280"))
                       ((>= (display-pixel-height) 2160)
                        (if (= 214 (caddr (assoc 'mm-size (frame-monitor-attributes))))
                            '("-*-Go Mono-regular-normal-normal-*-20-*-*-*-m-0-iso10646-1" 50 "display-pixel-height >= 2160 high and mm-size height = 214") ; 26 on Gnome and 24 on KDE Plasma?
                          '("-*-Go Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1" 70 "display-pixel-height >= 2160 high and mm-size height != 214"))) ; was 20?
                       ((> (display-pixel-height) 1080)
                        '("-*-Go Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1" 55 "display-pixel-height > 1080 high"))
                       ((= 170 (caddr (assoc 'mm-size (frame-monitor-attributes))))
                        '("-*-Go Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1" 45 "mm-size height = 170"))
                       ((= 340 (caddr (assoc 'mm-size (frame-monitor-attributes))))
                        '("-*-Go Mono-normal-normal-normal-*-26-*-*-*-m-0-iso10646-1" 50 "mm-size height = 340"))
                       (t
                        '("-*-Go Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1" 50 "everything else"))))))

(defun tkb-set-frame-font ()
  (interactive)
  (set-frame-font (cadr (assoc-string
			 (completing-read "Font? " tkb-fonts) tkb-fonts))))


(defvar tkb-default-tag nil)
(defvar tkb-default-font nil)
(defvar tkb-default-height nil)
(defvar tkb-default-description nil)
(defvar tkb-default-width 80)

(defun tkb-initial-font-and-size (prefix)
  (interactive "P")
  ;; Because I sometimes use this after emacs -q, and it needs cl.
  (require 'cl-lib)
  
  ;; need to do something with display-pixel-height
  (cl-destructuring-bind (tag font height description)
      (assoc-string (if prefix
			(completing-read "Font? " tkb-fonts)
		      (if nil "dejavu-13pt" "Go Mono"))
                    tkb-fonts)

    (setq tkb-default-tag tag)
    (setq tkb-default-font font)
    (setq tkb-default-height height)
    (setq tkb-default-description description)

    (setq tkb-default-top
	  (cl-case system-type
	    ((darwin) 30)
            ((gnu/linux)
             (if (string-equal (getenv "XDG_CURRENT_DESKTOP") "KDE")
                 10
               50))
	    (otherwise 20)))

    (when nil                           ; This was sadly naive.
      (if (>= (display-pixel-width) 2160)
          (setq tkb-default-left 1925)
        (setq tkb-default-left 5)))

    (message "initial frame monitor geometry: %S" (frame-monitor-geometry))

    (cond ((= 7680 (car (frame-monitor-geometry)))
           (setq tkb-default-left (+ 7680 10)))
          ((= 3840 (car (frame-monitor-geometry)))
           (setq tkb-default-left (+ 3840 10)))
          ((= 1920 (car (frame-monitor-geometry)))
           (setq tkb-default-left (+ 1920 10)))
          ((= 0 (car (frame-monitor-geometry)))
           (setq tkb-default-left 10))
          (t
           (setq tkb-default-left 10)))
          
    (let* ((dh (display-pixel-height))
	   (ch (frame-char-height))
	   (nfh (truncate (- (/ dh ch) (* (/ tkb-default-top ch) 2)
			     (* (/ dh ch) .10)))))
      (when nil (not (y-or-n-p (format "dh: %d ch: %d nfh: %d " dh ch nfh)))
	    (error "not doing it"))
      ;;  FIXME: make this work if executed multiple times.
      ;;(set-frame-parameter nil 'height nfh)
      (message "nfh: %d" nfh)
      ;;(set-frame-height nil nfh)
      (set-frame-font tkb-default-font nil t)
      (set-frame-height nil tkb-default-height)

      (let ((alt-color (getenv "EMACS_ALT_COLOR"))
            (color))
        (if alt-color
            (if (string-empty-p alt-color)
                (setq color "khaki1")
              (setq color alt-color))
          (setq color "wheat"))
        (setq tkb-default-frame-alist
	      `((width . ,tkb-default-width)
	        (height . ,tkb-default-height)
	        ;; (height . ,nfh)
	        (top . ,tkb-default-top)
	        (left . ,tkb-default-left)
	        (font . ,tkb-default-font)
	        ;; This makes things feel weird.
	        ;; (minibuffer . nil)
	        (background-color . ,color)
	        (foreground-color . "black")
	        (cursor-color . "orange")))
        (message "default-frame-alist: ")
        (pp default-frame-alist)
        (setq default-frame-alist
	      (append default-frame-alist tkb-default-frame-alist))
        (message "initial-frame-alist: ")
        (pp initial-frame-alist)
        (setq initial-frame-alist default-frame-alist)
        (tool-bar-mode -1)
        (set-frame-parameter nil 'font tkb-default-font)
        (set-frame-parameter nil 'top tkb-default-top)
        (set-frame-parameter nil 'left tkb-default-left)
        (set-frame-parameter nil 'height tkb-default-height)
        (set-frame-parameter nil 'width tkb-default-width)
        
        (message "(%d,%d), %d×%d with font %S "
                 tkb-default-left tkb-default-top
                 tkb-default-width tkb-default-height
                 tkb-default-font)))))

(tkb-initial-font-and-size nil)
(tkb-keys ((kbd "C-c k g i") 'tkb-initial-font-and-size))

(when nil 
  (defvar tkb-timer nil)
  (defun tkb-position-frame (&optional new-left)
    (interactive "P")
    (let* ((new-left (if new-left (prefix-numeric-value new-left) -50))
	   (dw (display-pixel-width))
	   (fw (frame-pixel-width))
	   (nl (- dw fw 50))
	   (top (frame-parameter nil 'top)))
      ;;(set-frame-position (selected-frame) nl top)
      ;;(set-frame-parameter nil 'left nl)
      ;;(frame-notice-user-settings)
      (set-frame-parameter nil 'left new-left)
      (set-frame-parameter nil 'top 20)
      (message "Yowza! %d %d %d %s" dw fw nl (current-time-string))
      (when (and (boundp 'tkb-timer) tkb-timer (memq tkb-timer timer-list))
	(cancel-timer tkb-timer))))
  ;;(add-hook 'window-setup-hook #'tkb-position-frame)
  ;;(set-frame-parameter nil 'left -50)
  ;;(set-frame-parameter nil 'left '(- 50))
  ;;(set-frame-parameter nil 'top 20)
  ;;(set-frame-position nil -50 20)

  (when nil
    (message "Setting timer: %s" (current-time-string))
    (setq tkb-timer
	  (run-at-time "10 seconds" nil #'tkb-position-frame -50)))

  (defun tkb-wait-frame ()
    (setq tkb-timer
	  (run-at-time "10 seconds" nil #'tkb-position-frame -50)))


  ;;(add-hook 'emacs-startup-hook #'tkb-position-frame)
  (add-hook 'emacs-startup-hook #'tkb-wait-frame))

(setq tkb-is-root (string-equal (user-real-login-name) "root"))

(setq frame-title-format
      '(multiple-frames
	("emacs %b: " (tkb-is-root (:eval (concat "root@" (system-name)))  user-mail-address))
	("emacs " (tkb-is-root (:eval (concat "root@" (system-name))) user-mail-address))))

(setq icon-title-format
      '(multiple-frames ("emacs %b: " (tkb-is-root "root" user-mail-address))
			("emacs " (tkb-is-root "root" user-mail-address))))
(message "Ending tkb-gui-setup.el")
;;; end of tkb-gui-setup.el
