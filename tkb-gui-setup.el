;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tkb-gui-setup.el -- Setup things that only work under a GUI.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Starting tkb-gui-setup.el")

(setq tkb-fonts
      `(("courier-11pt" ;; w32 says this is 11pt
	 "-outline-Courier New-normal-r-normal-normal-15-*-*-*-c-*-iso10646-1"
	 55)
	("dejavu-9pt" ;; w32 says this is 9pt
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-12-90-96-96-c-*-iso10646-1"
	 55)
	("dejavu-10pt" ;; w32 says this is 10pt
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1"
	 55)
	("dejavu-11pt" ;; w32 says this is 11pt
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-15-112-96-96-c-*-iso10646-1"
	 55)
	("dejavu-13pt" ;; w32 says this is 13 point; is it iso10646-1 by default?
	 "-*-DejaVu Sans Mono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
	 50)
	;; ftp://ftp.gnu.org/pub/gnu/freefont/
	;; http://www.gnu.org/software/freefont/
	("freefont-14pt" ;; w32 says this is 14 point; wider than dejavu 13pt
	 "-outline-FreeMono-normal-r-normal-normal-19-142-96-96-c-*-iso10646-1"
	 50)
	("freefont-13pt" ;; w32 says this is 13 point
	 "-outline-FreeMono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
	 55)
	("unifont"
	 ;; w32 doesn't show this in font selection dialog...
	 ;; Windows Font Explorer shows 12 pt as best looking size.
	 "-outline-unifont-medium-r-normal-normal-*-*-96-96-p-*-iso10646-1"
	 65)
	
	;; Ok if don't need unicode:
	("dina" "-raster-Dina-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1" 55)
	("consolas" "-outline-Consolas-normal-r-normal-normal-17-127-96-96-c-*-iso8859-1" 55)
	("inconsolata" "-outline-Inconsolata-medium-r-normal-normal-17-127-96-96-c-*-iso8859-1" 55)

	("Go Mono" ,@(cond
                      ((>= (display-pixel-height) 2160)
                       '("-*-Go Mono-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1" 58))
                      ((> (display-pixel-height) 1080)
                       '("-*-Go Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1" 55))
                      (t
                       '("-*-Go Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1" 50))))
	))

(defun tkb-set-frame-font ()
  (interactive)
  (set-frame-font (cadr (assoc-string
			 (completing-read "Font? " tkb-fonts) tkb-fonts))))


(defun tkb-initial-font-and-size (prefix)
  (interactive "P")
  ;; Because I sometimes use this after emacs -q, and it needs cl.
  (require 'cl-lib)
  
  ;; need to do something with display-pixel-height
  (destructuring-bind (tag tkb-default-font tkb-default-height)
      (assoc-string (if prefix
			(completing-read "Font? " tkb-fonts)
		      (if nil "dejavu-13pt" "Go Mono"))
                    tkb-fonts)

    (setq tkb-default-top
	  (cl-case system-type
	    ((darwin) 30)
            ((gnu/linux) 50)
	    (otherwise 20)))
    (set-frame-font tkb-default-font)

    (setq tkb-default-left 50)

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
      (set-frame-height nil tkb-default-height)
      (setq tkb-default-frame-alist
	    `((width . 80)
	      (height . ,tkb-default-height)
	      ;; (height . ,nfh)
	      (top . ,tkb-default-top)
	      (left . ,tkb-default-left)
	      (font . ,tkb-default-font)
	      ;; This makes things feel weird.
	      ;; (minibuffer . nil)
	      (background-color . "wheat")
	      (foreground-color . "black")
	      (cursor-color . "orange")))
      (setq default-frame-alist
	    (append default-frame-alist tkb-default-frame-alist))
      (setq initial-frame-alist default-frame-alist)
      (tool-bar-mode -1)
      ;;(set-frame-parameter nil 'height tkb-default-height)
      (set-frame-parameter nil 'font tkb-default-font)
      (set-frame-parameter nil 'top tkb-default-top)
      (set-frame-parameter nil 'left tkb-default-left))))

(tkb-initial-font-and-size nil)

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
