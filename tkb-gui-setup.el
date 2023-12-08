;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tkb-gui-setup.el -- Setup things that only work under a GUI.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "Starting tkb-gui-setup.el")

;; originals were: Courier, DejaVu Sans Mono, Freefont, Unifont

(cl-defun tkb-mf (&optional
                    (point-size tkb-default-point-size)
                    (font-name tkb-default-font-name))
  "Make a font from a font name and a font size."
  (format "%s-%d" font-name point-size))

(defvar tkb-default-width 80
  "The default width in characters for emacs frames.")
(defvar tkb-default-point-size 10)

(defvar tkb-default-font-name "IBM Plex Mono")
(defvar tkb-default-font nil
  "The default font for emacs frames chosen by tkb-gui-setup.el.")
(defvar tkb-default-height nil
  "The default height for emacs frames chosen by tkb-gui-setup.el." )
(defvar tkb-default-description nil
  "The description of the conditions that were met to produce this set of
defaults.")
(defvar tkb-default-top nil
  "The default y-offset chosen for emacs frames by tkb-gui-setup.el.")
(defvar tkb-default-left nil
  "The default x-offset chosen for emacs frames by tkb-gui-setup.el.")
(defvar tkb-default-color nil 
  "The default color chosen for emacs frames by tkb-gui-setup.el.")
(defvar tkb-default-frame-alist nil
  "The default-frame-alist chosen for emacs frames by tkb-gui-setup.el")


(defun tkb-calculate-gui-defaults ()
  (interactive)
  
  ;; need to do something with display-pixel-height
  (cl-destructuring-bind (font height description)
      (cond
        ;; Maybe I should do something with
        ;; (assoc 'mm-size (frame-monitor-attributes)) ?
        ((>= (display-pixel-height) 2280)
         ;; Retina display probably, so use smaller font
         `(,(tkb- 56 "go display-pixel-height >= 2280")))
        ((>= (display-pixel-height) 2160)
         (if (>= 214 (caddr (assoc 'mm-size (frame-monitor-attributes))))
             `(,(tkb-mf 12) 50
                "display-pixel-height >= 2160 high and mm-size height <= 214")
           `(,(tkb-mf 12) 70
              "display-pixel-height >= 2160 high and mm-size height != 214")))
        ((> (display-pixel-height) 1080)
         `(,(tkb-mf) 55 "display-pixel-height > 1080 high"))
        ((= 170 (caddr (assoc 'mm-size (frame-monitor-attributes))))
         `(,(tkb-mf) 45 "mm-size height = 170"))
        ((= 340 (caddr (assoc 'mm-size (frame-monitor-attributes))))
         `(,(tkb-mf) 50 "mm-size height = 340"))
        (t
         `(,(tkb-mf) 50 "everything else")))
    (setq tkb-default-font font)
    (setq tkb-default-height height)
    (setq tkb-default-description description)
    (setq tkb-default-top
	  (cl-case system-type
	    ((darwin) 30)
            ((gnu/linux)
             (if (string-equal (getenv "XDG_CURRENT_DESKTOP") "KDE")
                 20
               80))
	    (otherwise 20)))

    (cond ((= 7680 (car (frame-monitor-geometry)))
           (setq tkb-default-left (+ 7680 20)))
          ((= 3840 (car (frame-monitor-geometry)))
           (setq tkb-default-left (+ 3840 20)))
          ((= 1920 (car (frame-monitor-geometry)))
           (setq tkb-default-left (+ 1920 20)))
          ((= 0 (car (frame-monitor-geometry)))
           (setq tkb-default-left 20))
          (t
           (setq tkb-default-left 20)))
    (let ((alt-color (getenv "EMACS_ALT_COLOR"))
          (color))
      (if alt-color
          (if (string-blank-p alt-color)
              (setq color "khaki1")
            (setq color alt-color))
        (setq color "wheat"))
      (setq tkb-default-color color))))

(tkb-calculate-gui-defaults)

(defun tkb-gui-report-defaults ()
  "Report the tkb-default-* values."
  (interactive)
  (message "\
tkb-default-description: %s
tkb-default-font:        %s
tkb-default-height:      %d
tkb-default-width:       %d
tkb-default-top:         %d
tkb-default-left:        %d
tkb-default-color:       %s"
           tkb-default-description
           tkb-default-font       
           tkb-default-height     
           tkb-default-width      
           tkb-default-top        
           tkb-default-left
           tkb-default-color)
  (cl-values))

(tkb-gui-report-defaults)


(defun tkb-initial-font-and-size (prefix)
  (interactive "P")
  ;; Because I sometimes use this after emacs -q, and it needs cl.
  (require 'cl-lib)

  (tool-bar-mode -1)

  (set-frame-font tkb-default-font nil t)
  (set-frame-height nil tkb-default-height)

  (setq tkb-default-frame-alist
	`(
          (width            . ,tkb-default-width)
	  (height           . ,tkb-default-height)
	  (top              . ,tkb-default-top)
	  (left             . ,tkb-default-left)
	  (font             . ,tkb-default-font)
	  (background-color . ,tkb-default-color)
	  (foreground-color . "black")
	  (cursor-color     . "orange")
          ))
  (setq default-frame-alist tkb-default-frame-alist)
  (setq initial-frame-alist tkb-default-frame-alist)

  (set-frame-parameter nil 'font tkb-default-font)
  (set-frame-parameter nil 'top tkb-default-top)
  (set-frame-parameter nil 'left tkb-default-left)
  (set-frame-parameter nil 'height tkb-default-height)
  (set-frame-parameter nil 'width tkb-default-width)

  ;;(set-frame-size (selected-frame) tkb-default-width tkb-default-height)

  )

(tkb-initial-font-and-size nil)
(tkb-keys ((kbd "C-c k g i") 'tkb-initial-font-and-size))


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
    (completing-read "Monospace Font? " fonts nil nil default)))

(defun tkb-select-frame-font ()
  (interactive)
  (let ((font (tkb-select-mono-font)))
    (if (not (string-blank-p font))
        (set-frame-font font)
      (message "No font selected!"))))

(setq tkb-is-root (string-equal (user-real-login-name) "root"))

(setq frame-title-format
      '(multiple-frames
	("emacs %b: " (tkb-is-root (:eval (concat "root@" (system-name)))  user-mail-address))
	("emacs " (tkb-is-root (:eval (concat "root@" (system-name))) user-mail-address))))

(setq icon-title-format
      '(multiple-frames ("emacs %b: " (tkb-is-root "root" user-mail-address))
	("emacs " (tkb-is-root "root" user-mail-address))))


(defvar tkb-beep-sound "/usr/share/sounds/freedesktop/stereo/bell.oga")
(defvar tkb-beep-program "ogg123")

(defun tkb-bell ()
  (interactive)
  (start-process "Beep" nil tkb-beep-program
                 tkb-beep-sound))

(setq ring-bell-function #'tkb-bell)

(unless (file-exists-p tkb-beep-sound)
  (yes-or-no-p (format "Error: tkb-beep-sound is set to \"%s\", which does \
not  exist!\nUnderstand? "
           tkb-beep-sound)))
(let ((path (split-string (getenv "PATH") ":")))
  (unless (file-installed-p tkb-beep-program path)
    (yes-or-no-p (format "Error: tkb-beep-sound is set to \"%s\", which does \
not exist!\nUnderstand? "
             tkb-beep-program))))


(message "Ending tkb-gui-setup.el")
;;; end of tkb-gui-setup.el
