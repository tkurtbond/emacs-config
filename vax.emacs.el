;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;!> .EMACS -- GNU Emacs (v18.41) initialization file for TKB.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;Adapt to various opsys and terminal keymappings;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((eq system-type 'vax-vms)
  ;;Combat problems with C-s and C-q on VMS
  (set-input-mode nil t)		;Make emacs understand flow control
  ;;Now make a translate table to translate the keyboard entered keys
  (let ((i 0)
	(the-table (make-string 128 0)))
    (while (< i 128)
      (aset the-table i i)
      (setq i (1+ i)))
    (aset the-table ?\037 ?\C-s)	;Map Ctrl-/ to Ctrl-S (US)
    (aset the-table ?\036 ?\C-q)	;Map Ctrl-^ to Ctrl-Q
    (aset the-table ?\034 ?\C-q)	;Map Ctrl-4 to Ctrl-Q (FS)
    (setq keyboard-translate-table the-table)
    ))
 ((eq system-type 'aix-v3)
  (if (or (string-equal "ibm3151" (getenv "TERM"))
	  (string-equal "tvi925" (getenv "TERM")))
      (let ((i 0)
	    (the-table (make-string 128 0)))
	(while (< i 128)
	  (aset the-table i i)
	  (setq i (1+ i)))
	(aset the-table ?\177 ?\C-h)
	(aset the-table ?\C-h ?\177)
	(setq keyboard-translate-table the-table))))
 )

;;;;;;;;;;Set Variables;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons "/emacs_library/local/" load-path))
(setq TeX-mode-hook 'turn-on-auto-fill)	;Turn on auto-fill in TeX or LaTeX
(setq outline-mode-hook 'turn-on-auto-fill) ;auto-fill in outlines
(setq text-mode-hook
      (function
       (lambda () 
	 (if (eq major-mode 'text-mode) ;only turn on auto-fill if text-mode,
	     (auto-fill-mode 1)))))	; *not* indented-text-mode
(setq mail-mode-hook 'turn-on-auto-fill)
(setq mail-self-blind t)		;auto-insert BCC

(setq basic-mode-hook
      (function
       (lambda ()
	 (abbrev-mode 1))))

;; K&R like indentation
;(setq c-mode-hook 'kr-c-mode-hook)

(setq icon-mode-hook
      (if t
	  (function
	   (lambda ()
	     (setq comment-column 40)
	     (setq comment-start "#")
	     (setq comment-start-skip "#+ *")
	     (setq comment-indent-hook 'tkb-icon-comment-indent)
	     (setq icon-indent-level 2)
	     (setq icon-continued-statement-offset 2)
	     (setq icon-brace-offset 0)))
	(function
	 (lambda ()
	   (setq comment-column 40)
	   (setq icon-brace-offset -4)))))

(defun tkb-icon-comment-indent ()
  (if (looking-at "###")		;never move these
      (current-column)
    (if (looking-at "##")		;leave alone for now
	(calculate-icon-indent)
      (if (looking-at "^#")
	  0				;leave alone if at beginning of line
	(skip-chars-backward " \t")	;move to bol or first nonblank
	(max (if (bolp)
		 0			;don't want extra space if blank line
	       (1+ (current-column)))	;extra space between code and comment
	     comment-column)))))

(put 'do 'lisp-indent-hook 2)
(put 'dolist 'lisp-indent-hook 1)
(put 'dotimes 'lisp-indent-hook 1)
(put 'prog 'lisp-indent-hook 1)
(put 'while 'scheme-indent-hook 1)	;for xscheme

;;;I think the following makes EVERY file written out stream_LF.
;;;Unfortunately, VMS utilities don't understand VMS's own stream_LF
;;;format very well, so we don't use it.  We wish we could, though,
;;;because it makes everything *so* much simpler.
;(setq vms-stmlf-recfm t)		;Make new files stream_LF

(put 'eval-expression 'disabled nil)	;Allow us to use M-ESC expr evaluator
(put 'narrow-to-region 'disabled nil)	;allow us to use narrowing
(put 'narrow-to-page 'disabled nil)	;allow us to use narrowing

(setq-default indent-tabs-mode nil)

(setq dired-directory-command "DIRECTORY/SIZE/DATE/OWNER/WIDTH=(FILENAME=40,SIZE=6)")


;;;;;;;;;;Define Functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-file-read-only-other-window (filename)
  "Edit file FILENAME, in another window,
but don't save without confirmation.
Like find-file but marks buffer as read-only.
May create a new window, or reuse an existing one;
see the function display-buffer"
  (interactive "FFind file read-only in other window: ")
  (switch-to-buffer-other-window (find-file-noselect filename))
  (setq buffer-read-only t))

(defun kr-c-mode-hook ()
  (setq c-indent-level 4)
  (setq c-continued-statement-offset 4)
  (setq c-brace-offset -4)
  (setq c-argdecl-indent 4)
  (setq c-label-offset -4)
  (setq comment-column 40)
  (cond					;vms stuff
   ((eq system-type 'vax-vms)
    (make-local-variable 'vms-stmlf-recfm)
    (setq vms-stmlf-recfm t))))


(defun make-default-read-only (arg)
  (setq find-file-hooks
	;; Add this hook at the end, in case other hooks modify the buffer
	(nconc find-file-hooks (list 'set-read-only))))

(defun set-read-only ()
  (setq buffer-read-only t))

(setq command-switch-alist
      (cons '("-r" . make-default-read-only) command-switch-alist))


(defun vms-macro-comment-indent ()
  (if (looking-at ";;;")		;never move these
      (current-column)
    (if (looking-at ";;")		;leave alone for now
	(current-column)
      (if (looking-at "^;")
	  0				;leave alone if at beginning of line
	(skip-chars-backward " \t")	;move to bol or first nonblank
	(max (if (bolp)
		 0			;don't want extra space if blank line
	       (1+ (current-column)))	;extra space between code and comment
	     comment-column)))))


(defvar tkb-comment-string-1 "!!!\|^!") ;these don't move
(defvar tkb-comment-string-2 "!!")	;these stay at level of code
(defvar tkb-comment-string-3 "^!")	;these don't move

(defun tkb-comment-indent ()
  (if (looking-at tkb-comment-string-1)	;never move these
      (current-column)
    (if (looking-at tkb-comment-string-2) ;leave alone for now
	(current-column)
      (if (looking-at tkb-comment-string-3)
	  0				;leave alone if at beginning of line
	(skip-chars-backward " \t")	;else skip to bol or first nonblank
	(max (if (bolp)
		 0			;don't want extra space if blank line
	       (1+ (current-column)))	;extra space between code and comment
	     comment-column)))))

(defun tkb-right-justify ()
  (interactive)
  (let ((d (save-excursion
	     (end-of-line)
	     (- 79 (current-column)))))
    (save-excursion
      (if (> d 0)
	  (insert-char ? d)))))

(defun tkb-pad-string (s)
  "Pad string S with spaces between each character"
  (interactive "sString: ")
  (let ((l (length s)) (s2 (char-to-string (aref s 0))) (i 1))
    (while (< i l)
      (setq s2 (concat s2 " " (char-to-string (aref s i))))
      (setq i (1+ i)))
    (insert s2)))

(defun tkb-ibn-padded (up)
  "Insert Buffer Name with spaces between each character. With arg, upcase."
  (interactive "P")
  (tkb-pad-string (cond (up (upcase (buffer-name)))
			(t (buffer-name)))))

(defun tkb-ibn (up)
  "Insert Buffer Name. With arg, upcase."
  (interactive "P")
  (insert (cond (up (upcase (buffer-name)))
		(t (buffer-name)))))

(defun hanoi-9 ()			;for when we are very, very bored
  (hanoi 9))

(defun tkb-basic-error (rc)		;recenter if prefix arg not supplied
  (interactive "P")
  (search-forward "%BASIC-")
  (if (not rc)
      (recenter -1)))

(defun tkb-find-line-in-list (n)
    (interactive "NListing Line Number: ")
    (goto-char (point-min))
    (re-search-forward (concat "^[ \t]*" (int-to-string n)))) 

;;;
;;; check for a new file on suspend-resume
;;;	works with aid of KEPT_EDITOR.COM
(defun check-for-one-file ()
  (let ((file (getenv "EMACSARG")))
    (and (stringp file)
	 (not (string-equal file ""))
	 (find-file file))))

;;;
;;; check for one or more new files on suspend-resume
;;;	works with aid of KEPT_EDITOR_MANY.COM
(defun check-for-many-files ()
  (let* ((i 1)
	 (file (getenv (format "EMACSARG_%d" i))))
    (while (and (stringp file)
		(not (string-equal file "")))
      (find-file file)
      (setq i (1+ i))
      (setq file (getenv (format "EMACSARG_%d" i))))))

;(setq suspend-resume-hook 'check-for-one-file)
(setq suspend-resume-hook 'check-for-many-files)


(defun kpd-set ()
  "Puts keypad in application mode (default) or numeric mode."
  (interactive)
  ;; change this to the one you prefer as default: 'a' or 'n'
  (setq keypad-default-mode "a")
  (set 'keypad-mode
       (read-string "Put keypad in mode \(a or n\)\: " keypad-default-mode))
  (if (string= keypad-mode "a") (send-string-to-terminal "\e="))
  (if (string= keypad-mode "n") (send-string-to-terminal "\e>")))

(defun crypt-region (start end password)
   "Encrypt/Decrypt a region"
  (interactive "r\nsPassword: ")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((i 0)
	    (c 0))
	(while (< (point) (point-max))
	  (progn
	    (setq c (following-char))
	    ;; crypt/decrypt only printable characters.
	    (if (> c 31)
		(progn
		  (delete-char 1)
		  (insert (logxor c (logand (aref password i) 31)))
		  (setq i (1+ i))
		  (if (< i (length password)) (setq i 0)))
	      (forward-char 1))))))))


(defun crypt-buffer (password)
  "Encrypt/Decrypt a buffer."
  (interactive "sPassword: ")
  (crypt-region (point-min) (point-max) password))

(defun dewatcherize ()
  (interactive)
  (let ((user (user-login-name))
	(looping t))
    (while looping
      (message "time: %s: %s" user (current-time-string))
      (setq looping (sit-for 30)))))

(defun tkb-eval-last-sexp (arg)
  "Evaluate sexp before point; print value in minibuffer.
With argument, print output into current buffer."
  (interactive "P")
  (let* ((end (point))
	 (beg (let ((stab (syntax-table)))
		(unwind-protect
		    (save-excursion
		      (set-syntax-table emacs-lisp-mode-syntax-table)
		      (forward-sexp -1)
		      (point))
		  (set-syntax-table stab)))
	      (point)
	      nil)
	 (form (car (read-from-string (buffer-substring beg end))))
	 (val (eval form)))
    (if arg (prin1 val (current-buffer)))
    (message "result: %s" val)))

(defun tkb-insert-iso-date (&optional time)
  (interactive)
  (let* ((time (if time time (current-time-string)))
	 (day (substring time 8 10))
	 (_ (if (= (aref day 0) ?\ ) (aset day 0 ?0)))
	 (_ (if (= (aref time 0) ?\ ) (aset time 0 ?0)))
         (year (substring time 20 24))
         (monthname (substring time 4 7))
	 (month
	  (cdr
	   (assoc
	    monthname
	    '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03") ("Apr" . "04")
	      ("May" . "05") ("Jun" . "06") ("Jul" . "07") ("Aug" . "08")
	      ("Sep" . "09") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
	 (iso-date (concat year "-"  month "-" day)))
  (insert iso-date)))
(global-set-key "\C-cI" 'tkb-insert-iso-date)

(defun tkb-insert-iso-time (&optional time)
  (interactive)
  (let* ((time (if time time (current-time-string)))
	 (iso-time (substring time 11 19)))
    (insert iso-time)))

(defun tkb-insert-iso-datetime (&optional time)
  (interactive)
  (tkb-insert-iso-date time)
  (insert " ")
  (tkb-insert-iso-time time))



(defun abs (n)
  "Convert negative numbers to positive numbers."
  (if (< n 0)
      (* n -1)
    n))

(defun count-chars-region ()
  (interactive)
  (message "%d characters in region" (abs (- (point) (mark)))))

(defun what-column ()
  "Display what column the cursor is in."
  (interactive)
  (message "Current Column: %d" (current-column)))


(defun tkb-find-current-directory ()
  (interactive)
  (find-file (file-name-directory (buffer-file-name))))

;;;;;;;;;;Patch emacs functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sanitize-node-name (s)
  (substring s 1 -2))

;; This removes the underscore at the beginning which the VMS version 
;; of system-name in sysdep.c does not.
(defun system-name ()
  (let ((i-name (getenv "INTERNET_HOST_NAME"))
	(l-name (getenv "SYS$NODE")))
    (if (and i-name (not (string-equal i-name "")))
	i-name
      (sanitize-node-name l-name))))

(load "dired.el")                       ; So we can overwrite dired-view-file.

(defun dired-view-file ()
  "In dired, examine a file in readonly mode."
  (interactive)
  (if (file-directory-p (dired-get-filename))
      (dired (dired-get-filename))
    (find-file-read-only (dired-get-filename))))


;;;;;;;;;;Define Keys;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key "\e#" 'query-replace-regexp)
(global-set-key "\e:" (function eval-expression))
(global-set-key "\e\C-r" 'isearch-backward-regexp)

(global-set-key "\C-c\C-a" 'abbrev-mode)
(global-set-key "\C-ca" 'auto-fill-mode)
(global-set-key "\C-cb" 'insert-buffer)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cC" 'center-line)
(global-set-key "\C-cD" 'tkb-find-current-directory)
;;(global-set-key "\C-cd" 'delete-rectangle) ; removed so I can add a map on d.
(global-set-key "\C-cE" 'tkb-basic-error)
(global-set-key "\C-ce" 'tkb-right-justify)
(global-set-key "\C-c\C-e" 'send-sexp-to-ulisp)
(global-set-key "\C-cf" 'tkb-ibn)
(global-set-key "\C-cF" 'tkb-ibn-padded)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ci" 'indent-code-rigidly)
(global-set-key "\C-cl" 'what-line)
(global-set-key "\C-c\C-l" 'tkb-find-line-in-list)
(global-set-key "\C-cp" 'tkb-pad-string)
(global-set-key "\C-cr" '(lambda ()
			   (interactive)
			   (let ((x (point)) (y (mark)))
			     (message "Characters in Region: %d"
				      (- (max x y) (min x y))))))
(global-set-key "\C-c\C-r" 'tkb-right-justify)
;(global-set-key "\C-cs" 'copy-region-as-kill)
(global-set-key "\C-cs" 'tkb-eval-last-sexp)
(global-set-key "\C-cw" 'write-region)
(global-set-key "\C-cx" 'send-region-to-ulisp)
(global-set-key "\C-c=" 'count-chars-region)

(define-key ctl-x-4-map "r" 'find-file-read-only-other-window)
(define-key ctl-x-4-map "\C-r" 'find-file-read-only-other-window)

(define-key indented-text-mode-map "\177" 'backward-delete-char-untabify)

(defvar tkb-r-map (make-sparse-keymap))
(global-unset-key "\C-xr")
(global-set-key "\C-xr" tkb-r-map)
(define-key tkb-r-map " " 'point-to-register)
(define-key tkb-r-map "c" 'clear-rectangle)
(define-key tkb-r-map "d" 'delete-rectangle)
(define-key tkb-r-map "g" 'insert-register)
(define-key tkb-r-map "i" 'insert-register)
(define-key tkb-r-map "j" 'register-to-point)
(define-key tkb-r-map "k" 'kill-rectangle)
(define-key tkb-r-map "o" 'open-rectangle)
(define-key tkb-r-map "r" 'copy-rectangle-to-register)
(define-key tkb-r-map "s" 'copy-to-register)
(define-key tkb-r-map "y" 'yank-rectangle)

(defvar tkb-d-map (make-sparse-keymap))
(global-set-key "\C-cd" tkb-d-map)
(define-key tkb-d-map "i" 'tkb-insert-iso-date)


;;;;;;;;;;Set up abbreviations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-readable-p "~/abbrev.def")
    (quietly-read-abbrev-file "~/abbrev.def"))

;;;;;;;;;;Set up automatic modes for my use;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq completion-ignored-extensions 
      (append '(".o" ".icx" ".u1" ".u2" ".ln3" ".crb")
	      completion-ignored-extensions))
(setq auto-mode-alist
      (append '(("\\.bas$"  . basic-mode)
		("\\.inc$"  . basic-mode)
		("\\.icn$"  . icon-mode)
		("\\.ltx$"  . latex-mode)
		("\\.mail$" . text-mode)
		("\\.sdcl$" . sdcl-mode)
		)
	      auto-mode-alist))
(autoload 'icon-mode "icon"
	  "Mode for editing icon programs" t)
(autoload 'latex-mode "tex-mode"
	  "Mode for editing LaTeX documents" t)
(autoload 'sdcl-mode "/emacs_library/local/sdcl_mode"
	  "Mode for editing sdcl programs" t)
(autoload 'basic-mode "/emacs_library/local/vaxbasic"
	  "Mode for editing VAX Basic programs" t)
(autoload 'browse-mode "/emacs_library/local/browse-mode"
	  "Mode for browsing files" t)

(cond
 ((not (eq system-type 'vax-vms))
  (setq display-time-day-and-date t)
  (display-time)
  (server-start)
  (load-library "uncompress")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local variables:
;;; mode: emacs-lisp
;;; End:

