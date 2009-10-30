;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-experimental.el -- Experimental -*- coding-system: utf-8 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-experimental.el 1.1 Sun, 26 Mar 2000 15:10:50 -0500 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)

(defun tkb-unhex-buffer ()
  (interactive)
  (let* ((s (buffer-substring (point) (mark)))
	 (s* (url-unhex-string s t)))
    (kill-region (point) (mark))
    (insert s*)))

(defun tkb-rst-print-subheads ()
  (interactive)
  (with-work-buffer " *ReST heads"
      (loop for c
	    ;; recommended list from the rst quickref: http://tinyurl.com/47lkhk
	    across "=-`:'\"~^_*+#<>"
	    for i from 1
	    do (let* ((s (format "Depth %d" i))
		      (u (make-string (length s) c)))
		 (insert (format "%s\n%s\n\n" s u))))))

(defadvice mh-file-command-p (around tkb-mh-file-command-p-ad activate)
  (message "tkb-mh-file-command-p-ad ran at %s" (format-time-string "%r"))
  ad-do-it
  (when (not ad-return-value)
    (ad-set-arg 0 (concat (ad-get-arg 0) ".exe"))
    ad-do-it))

(progn

(put 'ucs-error 'error-conditions '(error tkb-errors ucs-error))
(put 'ucs-error 'error-message "UCS error")

(defun tkb-ucs-check (code-point)
  (let ((c (decode-char 'ucs code-point)))
    (if c
	c
      (if (or (< code-point 0) (> code-point #x10FFFF))
	  (signal 'ucs-error
		  (list
		   (format "U+%X is not a unicode character code" code-point)
		   code-point))
	(signal 'ucs-error
		(list
		 (format "Character U+%X is not yet supported" code-point)
		 code-point))))))

(defun tkb-ucs-insert (arg &optional base)
  "Insert the Emacs character representation of the given Unicode.
Interactively, prompts for a numeric string giving the code."
  (interactive
   (let* ((base (cond
		 ((null current-prefix-arg)     16)
		 ((consp current-prefix-arg)    10) ;C-u, arg is (<integer>)
		 ((integerp current-prefix-arg) current-prefix-arg)
		 (t
		  (error "Uninterpretable BASE %S\n" current-prefix-arg))))
	  (s (read-string (format "Unicode (base %d): " base)))
	  (n (string-to-number s base)))
     (list n base)))
  (if (null base) (setq base 16))
  (or (integerp arg)
      (setq arg (string-to-number arg base)))
  (insert (tkb-ucs-check arg)))
(tkb-keys ("\C-cKu" #'tkb-ucs-insert))

(defun tkb-superscript ()
  (interactive)
  (loop for c in '(#x2070 #x00B9 #x00B2 #x00B3 #x2074
		   #x2075 #x2076 #x2077 #x2078 #x2079)
	do (tkb-ucs-insert c)))

(defun tkb-subscript ()
  (interactive)
  (loop for c in '(#x2080 #x2081 #x2082 #x2083 #x2084
		  #x2085 #x2086 #x2087 #x2088 #x2089)
	do (tkb-ucs-insert c)))
;; (tkb-ucs-insert "2014") ; em dash
)

(defun frame-completions ()
  (loop for frame in (frame-list)
	collect (cons (frame-parameter frame 'name) frame) into frames
	finally return frames))

(defun tkb-select-frame ()
  (interactive)
  (let* ((frames (frame-completions))
	 (frame-name (completing-read "Frame? " frames))
	 (frame (cdr (assoc-string frame-name frames))))
    (raise-frame frame)
    (select-frame frame)))

(tkb-keys ((kbd "C-c A") #'tkb-select-frame))

(defun tkb-load-file ()
  (interactive)
  (load-file (buffer-file-name)))

(tkb-keys :keymap emacs-lisp-mode-map
	  ("\C-xL" #'tkb-load-file))

(tkb-keys ("\C-cko\C-c" #'org-ctrl-c-ctrl-c))
(tkb-check-bindings '("\C-cko\C-c"))

(when-load-dir (d "org/lisp")
  (add-to-list 'Info-default-directory-list
	       (expand-file-name (format "%s/../%s/" d "doc")))
  (require 'org-install)
  (setq org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3)))
  (setq org-agenda-files "~/current/org/.agenda_files")
  (setq org-columns-default-format "%30ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM{Total}")
  ;;(setq org-log-done (quote (done state clock-out)))

  (setq org-agenda-text-search-extra-files '("~/current/org/notes.org"))
  (setq org-agenda-custom-commands
	'(("H" "Office and Home Lists"
	   ((agenda)
	    (tags-todo "WORK")
	    (tags-todo "HOME")
	    (tags-todo "COMPUTER")
	    (tags-todo "DVD")
	    (tags-todo "READING")
	    (tags "CHARGE")))
	  ("D" "Daily Action List"
	   ((agenda "" ((org-agenda-ndays 1)
			(org-agenda-sorting-strategy
			 (quote ((agenda time-up priority-down tag-up) )))
			(org-deadline-warning-days 0)
			))
	    (tags "WORK")
	    (tags "CHARGE")
	    (tags "CLOCK"))))))

(when-load-dir (d "remember/")
  (add-to-list 'Info-default-directory-list d)
  (require 'remember)
  (org-remember-insinuate)
  (setq org-directory "~/current/org")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (autoload 'org-remember "org-remember" nil t)
  (setq org-remember-templates
	'(("Journal" ?j "* %^{Title} %U\n  %i\n  %?\n"
	   "~/current/org/journal.org" "Journal")
	  ("Contacts Log" ?c "* %^{Title} %U\n  %i\n  %?\n"
	   "~/current/org/contact-log.org" "Contact-Log")
	  ("Notes" ?n "\n\n* %^{Title} %U\n  %i\n  %?\n  %a\n\n"
	   "~/current/org/notes.org" "Notes")
	  ("Tasks" ?t "* TODO %^{Title} %U\n  %i\n  %?\n  %a"
	   "~/current/org/tasks.org" "Tasks")))
  (tkb-keys ("\C-ckor" #'org-remember)))

(progn
  (autoload 'daily "daily")
  (autoload 'daily-add-time-range-entry "daily")
  (tkb-keys ("\C-ckj" #'daily)
	    ("\C-ckA" #'daily-add-time-range-entry)))

(defun tkb-toggle-trailing-whitespace-display ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))


(defun increment-filename (prefix &optional suffix always start)
  "Generate a unique filename using PREFIX and optionally SUFFIX"
  (let* ((suffix (if suffix suffix ""))
	 (start (if start start 0))
	 (sep1   "_")
	 (sep2   "_")
	 (fileprefix (concat prefix)))
    (loop for i from start
	  ;; The zeroth filename doesn't have the number.
	  for testname = (if always
			     (format "%s%s%d%s" fileprefix sep2 i suffix)
			   (format "%s%s" fileprefix suffix))
	  then (format "%s%s%d%s" fileprefix sep2 i suffix)
	  until (not (file-exists-p testname))
	  finally return testname)))

(defun increment-filename-date (prefix &optional suffix always date)
  "Generate a unique filename using PREFIX and optionally SUFFIX"
  (let* ((suffix (if suffix suffix ""))
	 (sep1   "_")
	 (sep2   "_")
	 (date   (if date
		     (if (listp date)
			 (format-time-string "%F" date)
		       date)
		   (format-time-string "%F")))
	 (fileprefix (concat prefix sep1 date)))
    (loop for i from 0
	  ;; The zeroth filename doesn't have the number.
	  for testname = (if always
			     (format "%s%s%d%s" fileprefix sep2 i suffix)
			   (format "%s%s" fileprefix suffix))
	  then (format "%s%s%d%s" fileprefix sep2 i suffix)
	  until (not (file-exists-p testname))
	  finally return testname)))

(defun increment-filename-date-prefix (prefix &optional suffix always date)
  "Generate a unique filename using PREFIX and optionally SUFFIX"
  (let* ((suffix (if suffix suffix ""))
	 (sep1   "_")
	 (sep2   "_")
	 (date   (if date
		     (if (listp date)
			 (format-time-string "%F" date)
		       date)
		   (format-time-string "%F")))
	 (dirname (file-name-directory prefix))
	 (filename (file-name-nondirectory prefix))
	 (fileprefix (concat dirname date sep1 filename)))
    (loop for i from 0
	  ;; The zeroth filename doesn't have the number, unless ALWAYS
	  for testname = (if always
			     (format "%s%s%d%s" fileprefix sep2 i suffix)
			     (format "%s%s" fileprefix suffix))
	  then (format "%s%s%d%s" fileprefix sep2 i suffix)
	  until (not (file-exists-p testname))
	  finally return testname)))


(defun rename-buffer-uniquely ()
  ;; (rename-buffer x t) and (generate-new-buffer-name) only work if
  ;; buffers with the names to be avoided already exist.  I want
  ;; something so I can rename a buffer so I can have things like two
  ;; greps running at once.
  "Rename the current buffer to something that will be unique."
  (interactive)
  (rename-buffer (loop with bufname =
		       (progn
			 (string-match (rx (and
					    string-start
					    (group (+ anything))
					    (group
					     (? "[0-9]+"))
					    string-end))
				       (buffer-name)))
		       for i from 1
		       with newname = (format "%s-%d" bufname i)
		       while (get-buffer newname)
		       finally return newname)))


(when-load-dir (d "mozrepl")
  (message "Found mozrepl directory: %s" d)
  (progn
    ;; http://wiki.github.com/bard/mozrepl/emacs-integration
    (case 'moz
      ((moz)
       (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

       (add-hook 'javascript-mode-hook 'javascript-custom-setup)
       (defun javascript-custom-setup ()
	 (moz-minor-mode 1)))
      ((espresso)

       (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

       (add-hook 'espresso-mode-hook 'espresso-custom-setup)
       (defun espresso-custom-setup ()
	 (moz-minor-mode 1)))
      )))


(progn
  ;; See Info: (org)Activation.
  ;; The following lines are always needed.  Choose your own keys.
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (tkb-keys ("\C-cks" #'org-store-link)
	    ("\C-cka" #'org-agenda))
  (global-font-lock-mode 1)			; for all buffers
  (add-hook 'org-mode-hook 'turn-on-font-lock))	; org-mode buffers only


(setq longlines-show-hard-newlines t)
(progn
  ;; http://jfm3-repl.blogspot.com/2006/06/emacs-tricks-1-completion.html
  (require 'completion)
  (dynamic-completion-mode)
  (global-set-key (kbd "M-<return>") 'complete))

(unless window-system (load "tkb-tty-colors.el"))


(setq tkb-color-list '(("black" "white")
		       ("white" "black")
		       ("green" "black")
		       ("black" "green")
		       ("black" "wheat")
		       ("black" "navajowhite1")))
;;(rplacd (last tkb-color-list) tkb-color-list)
(defun tkb-toggle-colors ()
  (interactive)
  (let* ((fg (frame-parameter nil 'foreground-color))
	 (bg (frame-parameter nil 'background-color))
	 (colors (list fg bg))
	 (x (cadr (member colors tkb-color-list))))
    (if x
	(destructuring-bind (nfg nbg) x
	  (set-frame-parameter nil 'foreground-color nfg)
	  (set-frame-parameter nil 'background-color nbg))
      (destructuring-bind (nfg nbg) (car tkb-color-list)
	 (set-frame-parameter nil 'foreground-color nfg)
	 (set-frame-parameter nil 'background-color nbg)))))



(defun nothing ()
  (interactive)
  nil)

(unless window-system
  (xterm-mouse-mode 1))

(when nil
  ;; Apparently eval doesn't macroexpand the function part of (function ...)
  (defmacro lambda* (&rest rest)
    (let ((buf (get-buffer-create "*lambda**")))
      (pp rest buf)
      (let ((e (cl-transform-lambda rest nil)))
	(pp e buf)
	(let ((l `(lambda ,@(cdr e))))
	  (pp l buf)
	  (eval l)))))
  (funcall (lambda* (&optional &key a &key (b 2)) (list a b)) :a 1)
  ((lambda* (&optional &key a &key (b 2)) (list a b))))

(defun tkb-python-indent-statement ()
  (interactive)
  (let (b e)
  (save-excursion
    (python-beginning-of-statement)
    (setq b (point))
    (python-end-of-statement)
    (setq e (point))
    (python-indent-region b e))))


(when nil
  ;; This didn't work. 2008-12-01
  (defadvice compile (before tkb-compile-ad activate)
    (message "tkb's compile before")
    (let ((fullname (buffer-file-name)))
      (when (string-match ".*/myblog/entries/.*\\.rst$" fullname)
	(message "tkb's compile before: after string-match")
	(when (not (local-variable-p 'compile-command))

	  ;; compile does it in the current directory,
	  ;; so we don't need the full name.
	  (make-local-variable 'compile-command)
	  (setq compile-command (concat "blog2html" " "
					(file-name-nondirectory fullname))))))))(eval-after-load "python"
  '
  (progn
    (tkb-keys :keymap python-mode-map
	      ([(control meta ?q)] #'tkb-python-indent-statement))
    ))

;; This isn't working
(defadvice python-load-file (before tkb-py-load-file (file-name) activate)
  "Expand FILE-NAME using `expand-file-name'."
  (setq file-name (expand-file-name file-name)))


;; Should be part of MSWoe???
(setq potential-pythons
      ["c:/Python26/python.exe" "c:/Python25/python.exe" "python"])
(when-file (p potential-pythons)
  (setq python-command p
	python-python-command p))
(defun tkb-toggle-pythons ()
  (interactive)
  (let ((old python-command))
    (if (not (string-equal python-command "python"))
	(setq python-command "python")
      (setq python-command
	    (find-if #'file-executable-p
		     potential-pythons)))
    (message "Switching pythons from %s to %s" old python-command)))


(eval-after-load "rst.el"
  '(custom-set-faces
    '(rst-level-1-face ((t (:background "red"))) t)
    '(rst-level-2-face ((t (:background "blue"))) t)
    '(rst-level-3-face ((t (:background "yellow"))) t)
    '(rst-level-4-face ((t (:background "magenta"))) t)
    '(rst-level-5-face ((t (:background "white"))) t)))


(defun tkb-continue-line (n)
  (interactive "P")
  (if n
      (setq n (prefix-numeric-value n))
    (setq n 79))
  (let ((start-char (save-excursion (beginning-of-line)
				    (buffer-substring (point) (1+ (point))))))
    (save-excursion
      (end-of-line)
      (while (< (current-column) 80)
	(insert start-char)))))
(tkb-keys ("\C-ckc" #'tkb-continue-line))

(when-directory (d (expand-file-name "~/lib/data/fortunes"))
  (setq fortune-dir d))
(setq fortune-file "~/lib/data/fortunes")

(tkb-keys ("\C-ckh" #'hyperspec-lookup))


(autoload 'fortune-in-buffer "fortune")
(defun tkb-fortune-string (&optional file)
  (interactive
   (list
    (if current-prefix-arg
	(fortune-ask-file)
      fortune-file)))
  (save-excursion
    (fortune-in-buffer t file)
    (set-buffer fortune-buffer-name)
    (let* ((fortune (buffer-string)))
      (substitute ?\ ?\n fortune))))


(defconst tkb-random-me
  ["Crazy K Ranch"
   "Sorry to say"
   "Simple is as simple does"
   "WTF!"
   "I have SEEN the CONSING!!"
   "Yow!"
   "Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!"
   ])
(defun tkb-random-me ()
  (interactive)
  (aref tkb-random-me (random (length tkb-random-me))))


(modify-coding-system-alist 'file "\\.rst\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.org\\'" 'utf-8-unix)
;; This is for unison.  Unfortunately, you can't include the .unison.
(modify-coding-system-alist 'file "\\.prf\\'" 'utf-8-unix)

(autoload 'linum-mode "linum" "display line numbers in fringe")
;(setq linum-format "%03d")
;(setq linum-format 'dynamic)
(when nil
  (setq linum-format 'tkb-linum-format)
  (defface tkb-linum '((t (:background "green" :foreground "red"))) "tkb-linum")
  (defun tkb-linum-format (lineno)
    ;; This assumes it is called while on the line in question.
    (let (line-begin line-end width)
      (save-excursion
	(forward-line 0)
	(setq line-begin (point))
	(end-of-line)		      ;doesn't handle field boundaries
	(setq line-end (point)))
      (setq width (- line-end line-begin))
      (concat (propertize (format "%3d" lineno) 'face 'linum)
	      (if (<= width 80) "  " (propertize "▶▶" 'face 'tkb-linum))))))

;; http://www.neverfriday.com/sweetfriday/2008/06/emacs-tip-word-counting-with-a.html
(defun wc ()
  (interactive)
  (message "Word count: %s" (how-many "\\w+" (point-min) (point-max))))

(when nil
  ;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
  (require 'ido)
  (ido-mode t)

  (setq ido-execute-command-cache nil)

  (defun ido-execute-command ()
    (interactive)
    (call-interactively
     (intern
      (ido-completing-read
       "M-x "
       (progn
	 (unless ido-execute-command-cache
	   (mapatoms (lambda (s)
		       (when (commandp s)
			 (setq ido-execute-command-cache
			       (cons (format "%S" s) ido-execute-command-cache))))))
	 ido-execute-command-cache)))))

  (add-hook 'ido-setup-hook
	    (lambda ()
	      (setq ido-enable-flex-matching t)
	      (global-set-key "\M-x" 'ido-execute-command)))
  )




(defmacro* eval-after-load* (file varlist &rest body)
  "Like `eval-after-load', but bind variables according to VARLIST in
the current environment of the `eval-after-load' expression, not the
environment when BODY is evaluated.  This allows easy passing of values
into BODY.
Each element of VARLIST is a symbol (which is bound to the current value
of that symbol) or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the
value of VALUEFORM in the environment of the `eval-after-load' expression.

A difference with `eval-after-load' is that BODY doesn't have to be quoted."
  `(eval-after-load ,file
     '(let ,(loop for v in varlist
		  collect (if (symbolp v)
			      `(,v ,(eval v))
			    `(,(car v) ,(eval (cadr v))))
		  into new-varlist
		  finally return new-varlist) ,@body)))
(put 'eval-after-load* 'lisp-indent-function
     (1+ (get 'eval-after-load 'lisp-indent-function)))

(when (file-directory-p "/sw/versions/viewmail/bzr")
  (setq load-path (remove-if (lambda (e)
			       (string-match "/vm/*$" e)) load-path))
  (add-to-list 'load-path
	       "/sw/versions/viewmail/bzr/share/emacs/site-lisp/vm")
  (require 'vm-autoloads)
  (setq vm-mime-text/html-handler 'emacs-w3m)
  (add-to-list 'Info-default-directory-list
	       "/sw/versions/viewmail/bzr/share/info"))

(when-directory (d "/sw/versions/wl/snap/info")
  (add-to-list 'Info-default-directory-list d))

(when nil
  (loop for e in load-path
	if (string-match "/vm/*$" e) append e into matches
	finally return matches))

(defun tkb-goto-info-node (arg)
  "Goto an Info node in normal text, specified as \"Info: (FILE)Node.\"
where the \"FILE\" is optional and the \".\" can also be a \",\"."
  (interactive "P")
  (require 'info)
  (save-excursion
    (when (re-search-forward "Info: \\(\\((.*)\\)*[^.,]*\\)" nil t)
      (let ((s (match-string 1)))
	(when (y-or-n-p (concat "Goto Info node " s " ? "))
	  (Info-goto-node s arg))))))
(tkb-keys ("\C-cki" #'tkb-goto-info-node))
;; Info: (info)Go to node



;; http://article.gmane.org/gmane.emacs.macintosh.osx/107
(standard-display-8bit 128 255)


;; http://nex-3.com/posts/45-efficient-window-switching-in-emacs
(tkb-keys :just-warn
  ([M-left]  'windmove-left)		; move to left windnow
  ([M-right] 'windmove-right)		; move to right window
  ([M-up]    'windmove-up)		; move to upper window
  ([M-down]  'windmove-down)		; move to downer window
  )


(defun tkb-move-frame-left ()
  (interactive)
  (let* ((left (frame-parameter nil 'left))
	 (width (frame-pixel-width)))
    (set-frame-parameter nil 'left (- left width 20))))
(tkb-keys ("\C-ckl" #'tkb-move-frame-left))

(defadvice browse-url (before tkb-advise-browse-url activate)
  (let ((all-args (ad-get-args 0))
	(rest-args (ad-get-args 1)))
    (when (and (not rest-args)
	       (boundp 'fork)
	       (symbol-value 'fork))
      (ad-set-args 1 (symbol-value 'fork)))
    (message "tkb-advise-browse-url: bound: %S args: %S" (boundp 'fork) args)))


(defun tkb-cleanup-title (title)
  (let ((s
	 (substitute ?- 32
		     (remove-if-not #'(lambda (c) (or (memq c '(?- 32 ?.))
						      (and (<= ?a c)
							   (<= c ?z))
						      (and (<= ?0 c)
							   (<= c ?9))))
				    (downcase title)))))
    (while (string-match "\\([^[:alnum:]]\\)\\1+" s)
      (let ((c (match-string 1 s))
	    (b (match-beginning 0))
	    (e (match-end 0)))
	(setq s (concat (substring s 0 (1+ b)) (substring s e)))))
    s))

(defun tkb-insert-rst-section (title char &optional above)
  (let* ((s (tkb-rst-section-underline title char)))
    (when above (insert s "\n"))
    (insert title "\n" s "\n")))

(defun tkb-rst-section-underline (title char)
  (make-string (length title) char))


(defvar tkb-journal-base-dir (file-name-as-directory "~/tkb/journal"))

(defun tkb-journal (title date &optional no-entry)
  (interactive (list (read-from-minibuffer "Title: ")
		     (tkb-get-date-from-user nil t)
		     current-prefix-arg))
  (message "no-entry: %S" no-entry)
  (let* ((journal-directory
	  (expand-file-name (concat
			     tkb-journal-base-dir
			     (format-time-string "%Y/"
						 date))))
	 (filename (format-time-string "%m.rst"))
	 (pathname (concat journal-directory filename))
	 (exists (file-exists-p pathname))
	 (today (format-time-string "%Y/%m/%d, %A"))
	 (today-underline (tkb-rst-section-underline today ?*)))
    ;; Create the directory for the current year if necessary.
    (when (not (file-exists-p journal-directory))
      (make-directory journal-directory t))
    (find-file pathname)
    ;; Create the journal file for the current month if necessary.
    (unless exists
      ;; Need to add boilerplate.
      (tkb-insert-rst-section (format-time-string "%Y-%m: %B" date) ?# t)
      (insert  "\n:Author: \\T. Kurt Bond\n\n"))
    ;; Create the section for the current day if necessary.
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" (regexp-quote today) "\n"
				       (regexp-quote today-underline) "\n")
			       nil t)
      (goto-char (point-max))
      (insert "\n\n" today "\n" today-underline "\n"))
    (goto-char (point-max))
    (unless no-entry
      (unless (looking-back "\n\\{2,\\}")
	(insert "\n\n"))
      (tkb-insert-rst-section (concat (format-time-string "%H:%M:%S" date)
				      (if (and title (/= 0 (length title)))
					  (concat " " title) "")) ?=)
      )))




;; http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
;; describe-char-unicodedata-file
(let ((udf-url "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
      (udf-dest "~/tmp/UnicodeData.txt"))
  (if (file-readable-p udf-dest)
      (setq describe-char-unicodedata-file udf-dest)
    (eval-after-load* "tkb-last" (udf-dest udf-url)
      (when (y-or-n-p (format "You need to download %s ! Do it? " udf-url))
	;; Really weird: wget -O 'file' complains that file doesn't exist!
	(let ((cmd (format "cd ~/tmp/ && wget -O %s --progress=dot '%s' &" udf-dest udf-url)))
	  (message "cmd: %s" cmd)
	  (shell-command cmd)
	  (setq describe-char-unicodedata-file "~/tmp/UnicodeData.txt"))))))

(define-minor-mode tkb-smart-unicode-mode
  "Toggle smart unicode punctuation" nil " ♻⚔☣☥☸◉⅙✽☮" ; "✘▧▧⚅☑☢☹☺♠♥♦♣♨"
  '(("\"" . unicode-smart-double-quote)
    ("'"  . unicode-smart-single-quote)
    ("-"  . unicode-smart-hyphen)
    ("."  . unicode-smart-period)))

;; http://www.ossh.com/emacs/imap/howto.html
(defun gnus-browse-imaps-server (server)
  "Browse a mail server in Gnus via IMAP-SSL."
  (interactive "sServer name: ")
  (gnus-group-browse-foreign-server
   (list 'nnimap server
	 (list 'nnimap-address server)
	 '(nnimap-stream ssl)
	 '(nnimap-list-pattern ("INBOX" "mail/*" "Mail/*" "INBOX.*"))
	 '(nnimap-expunge-on-close ask))))

(defun fmt-duration (time)
  (flet ((f (duration unit)
	    (when duration (format (if (floatp duration)
				       "%f%s"
				     "%d%s") duration unit))))
    (destructuring-bind (hi lo ms) time
      (let ((s (+ hi lo))
	    (x "")
	    (d nil)
	    (h nil)
	    (m nil))
	(when (>= s 86400)
	  (setq d (/ s 86400)
		s (% s 86400)))
	(when (>= s 3600)
	  (setq h (/ s 3600)
		s (% s 3600)))
	(when (>= s 60)
	  (setq m (/ s 60)
		s (% s 60)))
	(unless (zerop ms)
	  (setq s (+ s (/ ms 1000000.0))))
	(mapconcat 'identity
		   (remove-if #'null
			      (list (f d "d") (f h "h") (f m "m") (f s "s")))
		   " ")))))


(defmacro time (&rest body)
  `(let ((start (current-time)))
     (unwind-protect
	 (progn ,@body)
       (let* ((end (current-time))
	      (delta (time-subtract end start))
	      (timebuf (get-buffer-create " *timing*")))
	 (display-buffer timebuf)
	 (princ (format "%s\n" (fmt-duration delta)) timebuf)))))

(defun tkb-blog (title tags category date &optional title-prefix)
  "Create a blog entry, prompting for various values and creating the
appropriate directory structure."
  (interactive "sTitle: \nsTags: \nsCategory: \nsDate: ")
  (let* ((date-time (if (listp date)
			date
		      (if (empty-string-p date)
			  nil
			(tkb-parse-iso-date date))))
	 (no-spaces (tkb-cleanup-title title))
	 (filename (increment-filename
		    (concat "~/myblog/entries/" (if (zerop (length category))
						   ""
						 (concat category "/"))
			   no-spaces)
		    ".rst" nil))
	 (dirname (file-name-directory filename))
	 (published (format-time-string "#published %Y-%m-%d %H:%M:%S"
					date-time)))
    (if (not (file-directory-p dirname))
	(make-directory dirname 'and-parents))
    ;; FIXME: warn about existing files...
    (find-file filename)
    (if title-prefix
	(insert title-prefix))
    (insert title)
    (insert "\n")
    (insert published)
    (insert "\n")
    (unless (zerop (length tags))
      (insert (concat "#tags " tags))
      (insert "\n"))))

(defun tkb-reading (authors tags date)
  "Create a blog entry about my recent reading"
  (interactive "sAuthors: \nsTags: \nsDate: ")
  (let* ((date-time (if (empty-string-p date)
			(current-time)
		      (tkb-parse-iso-date date)))
	 (tags (concat "recent reading" (if (empty-string-p tags)
					    ""
					  (concat "," tags))))
	 (category (format-time-string "books/read/%Y/%m" date-time)))
    (tkb-blog authors tags category date-time "Recent Reading: ")))

(defun tkb-viewing (titles tags date)
  "Create a blog entry about my recent viewing"
  (interactive "sTitles: \nsTags: \nsDate: ")
  (let* ((date-time (if (empty-string-p date)
			(current-time)
		      (tkb-parse-iso-date date)))
	 (tags (concat "recent viewing" (if (empty-string-p tags)
					    ""
					  (concat "," tags))))
	 (category (format-time-string "media/viewing/%Y/%m" date-time)))
    (tkb-blog titles tags category date-time "Recent Viewing: ")))

(progn			       ; used by my hooks for rst and asciidoc
  (add-to-list 'load-path (expand-file-name "~/lib/emacs/others"))
  (load-library "unichars")
  (load-library "xmlunicode"))

(defun tkb-asciidoc-version-increment ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^v\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\),[ \t]+\\(.+\\)"
			     nil t)
      (let* ((part3 (match-string 3))
	     (date  (match-string 4))
	     (n     (string-to-number part3))
	     (new3  (format "%d" (1+ n))))
	(when (and (not (string-equal date (tkb-timestamp)))
		   (y-or-n-p (format "Sure replace %s with %s? " part3 new3)))
	  (replace-match new3 nil t nil 3)
	  (replace-match (tkb-timestamp) nil t nil 4))
	nil))))

(defun tkb-doc-mode-hook ()
  (add-hook 'write-contents-functions #'tkb-asciidoc-version-increment))

(progn
  (autoload 'doc-mode "doc-mode")
  (add-to-list 'auto-mode-alist '("\\.adc$" . doc-mode))
  (eval-after-load "compile"
    '(add-to-list 'compilation-error-regexp-alist
		  '("^ERROR:[ \t]*\\([[:alpha:]][-[:alnum:].]+\\):[ \t]*line[ \t]*\\([0-9]+\\):" 1 2)))
  (eval-after-load "doc-mode" '
    (progn
      (defun bind-doc-mode-keys ()
	(set-language-environment "utf-8")
	(define-key doc-mode-map "\"" 'unicode-smart-double-quote)
	(define-key doc-mode-map "'" 'unicode-smart-single-quote)
	(define-key doc-mode-map "-" 'unicode-smart-hyphen)
	(define-key doc-mode-map "." 'unicode-smart-period)
	;; display UniChar menu when in doc mode
	(define-key doc-mode-map [menu-bar unichar]
	  (cons "UniChar" unicode-character-menu-map))
	;; set input method to "xml" (xmlunicode) when in doc mode
	(set-input-method 'xml))
      (add-hook 'doc-mode-hook #'bind-doc-mode-keys)
      (add-hook 'doc-mode-hook #'tkb-doc-mode-hook))))

(defun tkb-describe-character (after)
  "Describe the character before point (after if a prefix was specified)
if it is a unicode character."
  (interactive "P")
  (let ((char (if after (char-after) (char-before))))
    (message "%S" (assoc (encode-char char 'ucs)
			 unicode-character-list))))
(tkb-keys ("\C-ckd" #'tkb-describe-character))



(progn
  ;; Yank from emacs in screen
  (defun tkb-yank-from-screened-emacs ()
    (interactive)
    (let ((text (current-kill 0)))
      (when (string-match "^\\(.*\\)\\\\\n[ \t]*\\(.*\\)$" text)
	(insert (match-string 1 text))
	(insert (match-string 2 text)))))
  ;; This doesn't handle multiple lines properly
  (defun tkb-cleanup-yank-from-screened-emacs ()
    (interactive)
    (let ((text (current-kill 0)))
      (while (string-match "\\`\\(.*\\)\\\\\n\\([[:ascii:][:nonascii:]]*\\)\\'" text)
	(setq text (concat (match-string 1 text) (match-string 2 text))))
      (kill-new text))))

(progn
  (defun tkb-fill-list ()
    (interactive)
    (save-excursion
      (let ((paragraph-start "\f\\|[	]*$\\|\\(?:[*\f]+\\)")
	    (paragraph-separate "[	\f]*$\\|\\(?:[*\f]+\\)"))
	(message "Buffer local variables: \n****\n%S\n****\n" (buffer-local-variables))
	(sit-for 1)
	(message "start: %S sep: %S" paragraph-start paragraph-separate)
	(sit-for 1)
	(mark-paragraph)
	(narrow-to-region (point) (mark))
	(sit-for 1)
	(fill-paragraph nil))))
  (tkb-keys ("\C-ckf" 'tkb-fill-list)))

;; Gimme some register compatibility binding love!
(tkb-keys
  ("\C-x/" 'point-to-register)		; C-x / became C-x r SPC
  ("\C-xj" 'jump-to-register)		; C-x j became C-x r j
  ("\C-xx" 'copy-to-register)		; C-x x became C-x r s
  ("\C-xg" 'insert-register)		; C-x g became C-x r i
  )

(setq list-faces-sample-text
      "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 {}[]<>()\/ ~`!@#$%^&*_-+=|;:'?")
(progn
  (defun tkb-shell-command-on-this-file ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (unless file-name
	(error "Buffer %s is not visiting a file" (buffer-name)))

      (let ((args (list (read-from-minibuffer "Shell command: "
					      (cons (concat " " file-name)
						    1)
					      nil
					      nil
					      'shell-command-history)
			current-prefix-arg
			shell-command-default-error-buffer)))
	(apply 'shell-command args))))
  (tkb-keys ("\C-ck!" 'tkb-shell-command-on-this-file)))


(fset 'tkb-nxml-end-of-element "\C-[\C-u\C-[\C-f")

(when-load-file "rng-auto" :load
		(setq auto-mode-alist
		      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
			    auto-mode-alist)))

(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map "\C-c\C-j" 'tkb-nxml-end-of-element)
     (setq nxml-sexp-element-flag t)))

(setq default-indicate-buffer-boundaries 'left) ;??? Too distracting???
(setq default-indicate-empty-lines t)

(defun bmi (height weight)
  "Calculate BMI, body mass index.  BMI <18.5 may be underweight, BMI of 18.5
to 25 may be optimal weight, BMI >25 may be overweight, BMI >30 is obese,
over 40 is morbidly obese."
  (interactive "nHeight in inches: \nnWeight in pounds: ")
  (let* ((bmi (* 703 (/ (float weight) (expt height 2))))
	 (characterization
	  (cond ((<  bmi 18.5) "underweight")
		((<= bmi 25.0) "optimal")
		((<  bmi 30.0) "overweight")
		((<  bmi 40.0) "obese")
		(t             "morbidly obese"))))
    (message "bmi: %06.3f; %s" bmi characterization)))


(when (not window-system)
  (eval-after-load "font-lock"
    '(progn
       (mapc #'(lambda (face)
		 (when (facep face)
		   (set-face-foreground face "cyan")))
	     '(font-lock-comment-face sgml-doctype-face))
       ;;(set-face-foreground 'font-lock-comment-face "cyan")
       ;;'sgml-doctype-face "cyan")
       (message "!!!cyan rules!!!"))))

;; Make numbered backups for files that already have numbered backups.
(progn
  (setq version-control nil)
  ;; Save backups in the .~ directory
  ;; Visual Studio failes to update VSWebCache when directories starting with
  ;; "." are present.
  (cond ((memq system-type '(windows-nt cygwin))
	 (setq backup-directory-alist '(("." . "_~"))))
	(t (setq backup-directory-alist '(("." . ".~"))))))

(defun tkb-next-blank-line ()
  (interactive)
  (while (not (looking-at "^[ \t]*$"))
    (forward-line)))
(tkb-keys ("\C-c\C-n" 'tkb-next-blank-line))

(defun tkb-previous-blank-line ()
  (interactive)
  (while (not (looking-at "^[ \t]*$"))
    (forward-line -1)))
(tkb-keys ("\C-c\C-p" 'tkb-next-blank-line))

(progn
  (defun tkb-yank-dired-filename ()
    (interactive)
    (let ((filename (dired-get-filename)))
      (kill-new filename)))
  (eval-after-load "dired"
    '(define-key dired-mode-map "F" 'tkb-yank-dired-filename)))

;; use ucs-insert to insert unicode characters.

;; Copy the current region and kill it escaped for common lisp.
(progn
  (defun tkb-copy-and-kill-for-common-lisp (beg end)
    (interactive "r")
    (let ((s (buffer-substring beg end))
	  (buf (get-buffer-create " *crazy-yank-for-common-lisp*")))
      (save-excursion
	(set-buffer buf)
	(delete-region (point-min) (point-max))
	(princ s buf)
	(goto-char 0)
	(while (search-forward "\"" nil t)
	  (backward-char)
	  (insert ?\\)
	  (forward-char 1))
	(kill-region (point-min) (point-max))
	;;(kill-buffer buf)
	)))
  (tkb-keys ("\C-ckL" 'tkb-copy-and-kill-for-common-lisp)))

(progn
  ;; FIXME: This should probably prompt for replacing???
  (defun fc-eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
	(prin1 (eval (read (current-kill 0)))
	       (current-buffer))
      (error (message "Invalid expression")
	     (insert (current-kill 0)))))
  (tkb-keys ("\C-cE" #'fc-eval-and-replace)))

;;; FIXME: C-u C-x C-e aka eval-last-sexp does this, without the
;;; prompt to see if you want to insert.  I should look at its
;;; implementation to see if this could be better???
(defun tkb-eval-backward-and-insert (stringifyp)
  "Insert value of the preceding sexp."
  (interactive "P")
  (let (b
	e
	(output-format (if stringifyp "%s" "%S")))
    (save-excursion
      (backward-sexp)
      (setq b (point))
      (forward-sexp)
      (setq e (point)))
    (let ((xs (buffer-substring-no-properties b e)))
      (condition-case nil
	  (let* ((x (read xs))
		 (result (eval x)))
	    (when (y-or-n-p
		   (format (concat "Result: %S; Insert as "
				   output-format "? ") result result))
	      (insert (format output-format result))))
	(error (message "Invalid expression: %s" xs))))))

(defun tkb-eval-prompted-and-insert (stringifyp x)
  (interactive "P\nXLisp Expression: ")
  (let ((output-format (if stringifyp "%s" "%S")))
    (when (y-or-n-p
	   (format (concat "Result: %S; insert as " output-format "? ")
		   x x))
      (insert (format output-format x)))))

(defun tkb-eval-backward-and-insert-in-comment (stringifyp)
  "Insert value of the preceding sexp."
  (interactive "P")
  (let (b
	e
	(output-format (if stringifyp ";;=> %s" ";;=> %S")))
    (save-excursion
      (backward-sexp)
      (setq b (point))
      (forward-sexp)
      (setq e (point)))
    (let ((xs (buffer-substring-no-properties b e)))
      (condition-case nil
	  (let* ((tkb-ebaiicx (read xs))
		 (result (eval tkb-ebaiicx)))
	    (when (y-or-n-p
		   (format (concat "Result: %S; Insert as "
				   output-format "? ") result result))
	      (insert (format output-format result))))
	(error (message "Invalid expression: %s" xs))))))

(defun tkb-trim-buffer ()
  (interactive)
  (beginning-of-buffer)
  (save-excursion
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))
(tkb-keys ("\C-cT" 'tkb-trim-buffer))

(defun tkb-trim-region ()
  (interactive)
  (save-excursion
    (let ((start (min (point) (mark)))
	  (end (max (point) (mark))))
      (goto-char start)
      (while (re-search-forward "[ \t]+$" end t)
	(replace-match "" nil nil)))))

(progn
  (defun tkb-replace-with-calc (start end)
    (interactive "r\n")
    (let ((o (make-overlay start end)))
      (overlay-put o 'face 'highlight)
      (unwind-protect
	  (let* ((n (string-to-number (buffer-substring start end)))
		 (x (read-from-minibuffer (format "Expression with n = %s: " n)
					  nil read-expression-map t
					  'read-expression-history
					  )))
	    (kill-region start end)
	    (let ((r (number-to-string (eval x))))
	      (message "Result: %s" r)
	      (insert r)))
	(delete-overlay o))))
  (when nil (tkb-keys ("\C-ce" #'tkb-replace-with-calc))))

(progn
  ;; http://emacs.wordpress.com/2007/01/20/record-play-re-play/
  (tkb-keys
    ([f7]  'start-kbd-macro)
    ([f8]  'end-kbd-macro)
    ([f9]  'call-last-kbd-macro)))


;;; Connects as sysdba.
(defun tkb-oracle-sysdba ()
  (interactive)
  (let ((sql-user "sys")
					;(sql-database "nspcp")
	(sql-oracle-options (list "as sysdba")))
    (sql-oracle)))

(progn
  (defun tkb-next-sexp ()
    (interactive)
    (backward-up-list -1))

  (tkb-keys ("\C-c)" 'tkb-next-sexp)))


(progn
  (defun tkb-copy-buffer-name (whole)
    (interactive "P")
    (message "whole: %s numeric: %d" whole (prefix-numeric-value whole))
    (let ((fn (buffer-file-name)))
      (kill-new (if whole fn (file-name-nondirectory fn)))))
  (tkb-keys ("\C-cP" #'tkb-copy-buffer-name))
  (defun tkb-copy-downcase-buffer-name (whole)
    (interactive "P")
    (message "whole: %s numeric: %d" whole (prefix-numeric-value whole))
    (let* ((fn (buffer-file-name))
	   (fn (if whole fn (file-name-nondirectory fn))))
      (kill-new (downcase fn))))
  (tkb-keys ("\C-cY" #'tkb-copy-downcase-buffer-name))


  (defun tkb-lower-to-register (register start end)
    (interactive "cLowercase to register: \nr")
    (set-register register (downcase (buffer-substring start end))))
  (tkb-keys ("\C-c*" 'tkb-lower-to-register))
  )

(progn
  ;; http://emacs.wordpress.com/2007/01/22/killing-yanking-and-copying-lines/

  (defun jao-copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring"
    (interactive "p")
    (kill-ring-save (line-beginning-position)
		    (line-beginning-position (+ 1 arg)))
    (message "%d line%s copied" arg (if (= 1 arg) "" "s")))


  (defadvice yank (after indent-region activate)
    (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
					     c-mode c++-mode objc-mode
					     LaTeX-mode TeX-mode))
	(indent-region (region-beginning) (region-end) nil)))
  (tkb-keys ("\C-cy" 'jao-copy-line))
  )




(defun tkb-ispell-selection ()
  (interactive)
  (let ((word (x-get-selection-value)))
    (if word
	(let ((buf (get-buffer-create "Spell Selection")))
	  (switch-to-buffer buf)
	  (delete-region (point-min) (point-max))
	  (insert word)
	  (ispell-word))
      (message "no selection"))))

(tkb-keys ("\C-c$" (if (fboundp 'x-get-selection-value)
		       #'tkb-ispell-selection
		     #'(lambda () (interactive)
			 (message "\
Not under a window system, so you can't ispell the selection")))))


(when nil
  ;; Requires w3
  ;; http://www.emacswiki.org/cgi-bin/wiki/chmouel
  ;; Search Google at point:
  (defun my-search-google (w)
    "Launch google on the Word at Point"
    (interactive
     (list (let* ((word (thing-at-point 'symbol))
		  (input
		   (read-string (format "Google%s: "
					(if (not word)
					    ""
					  (format " (default %s)" word))))))
	     (if (string= input "")
		 (if (not word)
					;sinon input
		     (error "No keyword to search given") word) input))))
    (browse-url (format "http:/www.google.com/search?q=%s" w)))
  (tkb-keys ("\C-cj" #'my-search-google)))


(when-load-file "mailcrypt"
  (load-library "mailcrypt")
  (mc-setversion "gpg")

  (autoload 'mc-install-write-mode "mailcrypt" nil t)
  (autoload 'mc-install-read-mode "mailcrypt" nil t)
  (add-hook 'mail-mode-hook 'mc-install-write-mode)

  (add-hook 'vm-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-mail-mode-hook 'mc-install-write-mode))

(eval-after-load "rng-loc"
  '(progn
     (setq rng-schema-locating-files
	   (append rng-schema-locating-files
		   '("~/comp/xsl-website/website-schemas.xml")))))

(when nil
  (setq mail-user-agent 'mh-e-user-agent)
  (setq read-mail-command 'mh-rmail))

(when t
  ;; This requires using message-user-agent for composing mail.
  (setq mail-user-agent 'message-user-agent)
  (setq message-from-style 'angles)
  (defun check-attachments-attached ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* (
	     ;; Nil when message came from outside (eg calling emacs as editor)
	     ;; Non-nil marker of end of headers.
	     (internal-messagep
	      (re-search-forward
	       (concat "^" (regexp-quote mail-header-separator) "$") nil t))
	     (end-of-headers		; Start of body.
	      (copy-marker
	       (or internal-messagep
		   (re-search-forward "^$" nil t)
		   (point-min))))
	     (limit
	      (or (re-search-forward "^-- $" nil t)
		  (point-max)))
	     (old-case-fold-search case-fold-search))
	(unwind-protect
	    (progn
	      (goto-char end-of-headers)
	      (when (search-forward "attach" limit t)
		(goto-char end-of-headers)
		;; the word 'attach' has been used, can we find an
		;; attachment?
		(unless
		    (or (re-search-forward "^<#/" limit t)
			(y-or-n-p
			 "Found the word `attach' but no MIME attachment: send anyway?"
			 )
			(error "Aborted send")))))
	  (set-marker end-of-headers nil)))))
  (add-hook 'message-send-hook 'check-attachments-attached))


(when-directory (d ["/sw/versions/slime/cvs/slime/" "/sw/src/slime"])
  (add-to-list 'load-path d)
  (require 'slime)
  (tkb-keys
    ("\C-c;" 'slime-insert-balanced-comments)
    ("\C-c\M-;" 'slime-remove-balanced-comments))
  (setq slime-lisp-implementations
	`(,(when-exec-found (f ["/sw/versions/cygwin/clisp/2.47/bin/clisp.exe"
				"c:/PROGRA~1/clisp-2.47/clisp.exe"
				"c:/sw/versions/clisp-bin/clisp-2.41/clisp.exe"
				"clisp"])
	     (list 'clisp (list f)))
	  ,(when-exec-found (f ["c:/Program Files/Steel Bank Common Lisp/1.0.29/sbcl.exe"
				"sbcl"])
	     (setq inferior-lisp-program f)
	     (list 'sbcl (list f)))
	  ,(when-exec-found (f ["lisp"])
	     (list 'cmucl (list f)))))
  ;; add '(slime-repl) or '(slime-fancy)
  (slime-setup))




(when t
  (defun tkb-zap-to-char (arg char)
    "Kill up to and including ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
    (interactive "p\ncZap to char: ")
    (kill-region (point) (progn
			   (search-forward (char-to-string char) nil nil arg)
			   (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
			   (point))))
  (tkb-keys ("\C-cZ" 'tkb-zap-to-char)))


(when (eq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))
(when nil
  (progn
    (setq locale-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)))

(when nil
  ;; http://www.tbray.org/ongoing/When/200x/2003/09/27/UniEmacs
  (progn
    (defvar ongoing-char-choice
      '("Special characters"
	(""
	 ("ccedil"    #xe7)
	 ("copyright" #xa9)
	 ("degree"    #xb0)
	 ("dot"       #xb7)
	 ("eacute"    #xe9)
	 ("half"      "&#xbd;")
	 ("omacr"     "&#x14d;")
	 ("oouml"     #xe4)
	 ("uuml"      #xfc)
	 ("euro"      #x20ac)
	 ("cents"     #xa2)
	 ("egrave"    #xe8)
	 ("lsquo"     #x2018)
	 ("rsquo"     #x2019)
	 ("ldquo"     #x201c)
	 ("rdquo"     #x201d)
	 ("mdash"     #x2014))))

    (defun ong-special-chars-menu ()
      "Insert a special character from a menu"
      (interactive)
      (let ((value
	     (car (x-popup-menu
		   (list '(10 10) (selected-window))
		   ongoing-char-choice))))
	(cond
	 ((integerp value) (ucs-insert value))
	 ((stringp  value) (insert value))
	 ('t )))) ;; so you can hit escape and make the menu go away


    (defun one-quote () "" (interactive) (insert ?'))
    (defvar sq-state 'nil "In single-quotes?")
    (defvar dq-state 'nil "In double quotes?")
    (defun ong-insert-special (c) "Insert special characters, like so:
 s => open/close single quotes
 d => open/close double quotes
 ' => apostrophe
 a => <a href=
 i => <img src=
 & => &amp;
 < => &lt;
 - => mdash
 . => center-dot"
      (interactive "c" "'")
      (cond
       ((= c ?s)
	(if sq-state
	    (progn
	      (ucs-insert #x2019)
	      (setq sq-state 'nil))
	  (ucs-insert #x2018)
	  (setq sq-state 't)))
       ((= c ?d)
	(if dq-state
	    (progn
	      (ucs-insert #x201d)
	      (setq dq-state 'nil))
	  (ucs-insert #x201c)
	  (setq dq-state 't)))
       ((= c ?') (ucs-insert #x2019))
       ((= c ?a)
	(progn
	  (if (> (current-column) 0) (newline-and-indent))
	  (insert "<a href=\"\">")
	  (backward-char 2)
	  ))
       ((= c ?i)
	(progn
	  (if (> (current-column) 0) (newline-and-indent))
	  (insert "<img src=\"\" alt=\"\" />")
	  (backward-char 11)
	  ))
       ((= c ?&) (insert "&amp;"))
       ((= c ?<) (insert "&lt;"))
       ((= c ?-) (ucs-insert #x2014))
       ((= c ?.) (ucs-insert #xb7))))))


(when nil
  ;; From Daniel Barlow: http://ww.telent.net/diary/2003/1/#14.28515
  ;; this works for gnus.
  (defun check-attachments-attached ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* (
	     ;; Nil when message came from outside (eg calling emacs as editor)
	     ;; Non-nil marker of end of headers.
	     (internal-messagep
	      (re-search-forward
	       (concat "^" (regexp-quote mail-header-separator) "$") nil t))
	     (end-of-headers		; Start of body.
	      (copy-marker
	       (or internal-messagep
		   (re-search-forward "^$" nil t)
		   (point-min))))
	     (limit
	      (or (re-search-forward "^-- $" nil t)
		  (point-max)))
	     (old-case-fold-search case-fold-search))
	(unwind-protect
	    (progn
	      (goto-char end-of-headers)
	      (when (search-forward "attach" limit t)
		(goto-char end-of-headers)
		;; the word 'attach' has been used, can we find an
		;; attachment?
		(unless
		    (or (re-search-forward "^<#/" limit t)
			(y-or-n-p
			 "Found the word `attach' but no MIME attachment: send anyway?"
			 )
			(error "Aborted send")))))
	  (set-marker end-of-headers nil)))))

  (add-hook 'message-send-hook 'check-attachments-attached))



(fset 'tkb-add-recent-reading
      "\C-s<itemizedlist>\C-a\C-@\C-[\C-f\C-m\C-c\C-rsection\C-m\C-[\C-a\C-m\C-c\C-etit\C-i\C-mRecent Reading\C-x\C-s")


;; Never use separate frames for ediff.  (Separate frames are totally useless
;; on tiled, tabbing window managers like ion.)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(autoload 'forth-mode "gforth")
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
			    auto-mode-alist))
(autoload 'forth-block-mode "gforth")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
			    auto-mode-alist))
(add-hook 'forth-mode-hook (function (lambda ()
				       ;; customize variables here:
				       (setq forth-indent-level 4)
				       (setq forth-minor-indent-level 2)
				       (setq forth-hilight-level 3)
;;; ...
				       )))


(eval-after-load "gforth"
  '(progn
     (defun forth-load-file (file-name)
       "Load a Forth file FILE-NAME into the inferior Forth process."
       (interactive (comint-get-source "Load Forth file: " forth-prev-l/c-dir/file
				       forth-source-modes t)) ; T because LOAD
					; needs an exact name
       (comint-check-source file-name) ; Check to see if buffer needs saved.
       (setq forth-prev-l/c-dir/file (cons (file-name-directory    file-name)
					   (file-name-nondirectory file-name)))
       (comint-send-string (forth-proc) (concat "include "
						file-name
						"\n")))))


(defun tkb-insert-name ()
  (interactive)
  (insert "T. Kurt Bond"))

(defun tkb-insert-e-mail ()
  (interactive)
  (insert user-mail-address))

;;(require 'prcs-hooks) ;; bloody intrusive.

;; Should I use x-display-pixel-height and x-display-pixel-width to move to
;; an entirely emacs-based geometry munging?
(cond
 (nil
  ;; Make new frames show up exactly on top of the old frame.
  ;; This is especially useful with focus-follows-pointer mode
  ;; and scrolling desktops.
  (let* ((params (frame-parameters))
	 (top (assq 'top params))
	 (left (assq 'left params))
	 (height (assq 'height params))
	 (width (assq 'width params)))
    (message "%s %s %s %s" top left height width)
    ;; Setting these seems only to work well with twm and close derivatives;
    ;; other window managers seem to offset the new windows from the old.
    (push top default-frame-alist)
    (push left default-frame-alist)
    (push height default-frame-alist)
    (push width default-frame-alist)
    (push '(user-position . t) default-frame-alist)))
 (nil
  (let* ((height (assq 'height (frame-parameters)))
	 (new-geometry `((top . 30)
			 (left . -75)
			 (width . 80)
			 ,height)))
    ;; Shouldn't I be replacing any matching items in the assoc?
    (cond (nil
	   (setq initial-frame-alist new-geometry)
	   (setq default-frame-alist (copy-list new-geometry)))
	  (t
	   (mapcar (lambda (item)
		     (push item initial-frame-alist)
		     (push item default-frame-alist))
		   new-geometry))))
  (tkb-position)))


(defun livejorunal-rss-url (name)
  (interactive "Mname:")
  (insert "\nhttp://www.livejournal.com/users/" name  "/data/rss\n"))

(defun livejorunal-rss-url (name)
  (interactive "MName:")
  (kill-new (concat  "http://www.livejournal.com/users/"
		     name  "/data/rss")))

(when (file-loadable-p "table")
  (autoload 'table-insert "table" nil t)
  (autoload 'table-recognize "table" nil t))


(when-load-file "rst"			;was when-load-dir
  (autoload 'rst-mode "rst"
    "mode for editing reStructuredText documents" t)
  (setq auto-mode-alist
	(append '(("\\.rst$" . rst-mode)
		  ("\\.rsti$" . rst-mode) ; include files.
		  ("\\.rest$" . rst-mode)) auto-mode-alist))
  (autoload 'rst-repeat-last-character "rst")
  (tkb-keys ("\C-cR" 'rst-repeat-last-character))
  (when nil			  ; replaced by tkb-smart-unicode-mode
    (eval-after-load "rst"
      '(progn
	 ;;(define-key rst-mode-map "'" #'unicode-smart-single-quote)
	 (define-key rst-mode-map "-" #'unicode-smart-hyphen)
	 (define-key rst-mode-map "\"" #'unicode-smart-double-quote))))
  
(defun tkb-rst-mode-hook ()
    (interactive)
    (unless window-system
      ;; rst-mode fontlock is unreadable with the colors screen uses.
      (make-local-variable 'rst-mode-lazy)
      (make-local-variable 'font-lock-support-mode)
      (setq rst-mode-lazy nil
	    font-lock-support-mode nil)
      (make-local-variable 'font-lock-mode)
      (font-lock-mode -1))
    (message "tkb-rst-mode-hook ran; font-lock-mode: %S" font-lock-mode)
    (tkb-smart-unicode-mode)
    (flyspell-mode 1)
    (cond
     ((string-match ".*/myblog/entries/.*\\.\\(rst\\|rsti\\)$" buffer-file-name)
      (set (make-local-variable 'compile-command)
	   (concat "pybloxrst "
		   (file-name-nondirectory buffer-file-name)
		   " >~/tmp/x.html"
		   (if (memq system-type '(ms-dos windows-nt cygwin))
		       " && shell2 ~/tmp/x.html"
		     ""))))
     (t
      (let ((pdf-name (concat
		       (file-name-nondirectory
			(file-name-sans-extension buffer-file-name))
		       ".pdf")))
	(set (make-local-variable 'compile-command)
	     (concat "make " pdf-name
		     (if (memq system-type '(ms-dos windows-nt cygwin))
			 (concat " && shell2 " pdf-name)
		       "")))))
    (add-hook 'before-save-hook 'time-stamp nil t)))
  (add-hook 'rst-mode-hook 'tkb-rst-mode-hook))

(fset 'tkb-mtnet-spam
      "zabuse@mountain.net, postmaster@mountain.net, help@mountain.net\C-s[att\C-a\C-o\C-o\C-xi~/tmp/mntnet\C-?\C-?\C-?\C-?\C-?tnet\C-i\C-m")


(setq Info-scroll-prefer-subnodes nil) ;scroll to end before jumping to subnodes

(defun tkb-prcs-add-log-file-name (buffer-file-name)
  (if (string-match "\\.prg$" buffer-file-name)
      nil
    ;; Do the default...
    (if (string-match
	 (concat "^" (regexp-quote (file-name-directory
				    file-name)))
	 buffer-file-name)
	(substring buffer-file-name (match-end 0))
      (file-name-nondirectory buffer-file-name))))
(setq add-log-file-name-function (function tkb-prcs-add-log-file-name))


(when (file-loadable-p "cpb/personal-log")
  (setq personal-log-dir "~/current")

  (load "cpb/personal-log")
  (defun tkb-eulisp-log (read)
    (interactive "P")
    (let ((personal-log-file (expand-file-name "working-notes.text"
					       "~/comp/eulisp")))
      (personal-log read)))

  (defun tkb-private-log (read)
    (interactive "P")
    (let ((personal-log-dir "~/tkb/"))
      (let ((personal-log-file (personal-log-calc-file)))
	(personal-log read))))

  (tkb-keys
    ("\C-cle" 'tkb-eulisp-log)
    ("\C-clh" 'personal-log-here)
    ("\C-clP" 'tkb-private-log)))

;; Do it in the X resources instead.
;;(tool-bar-mode -1)
;;(scroll-bar-mode -1)


(defun tkb-vm-visit-file (folder read-only)
  (interactive "fVM File: \nP")
  (vm-visit-folder folder read-only))
(tkb-keys ("\C-cv" 'tkb-vm-visit-file))


(defun tkb-dec-to-hex (n)
  (interactive "NDec: ")
  (message "0x%x" n))
(defun tkb-hex-to-dec (n)
  (interactive "sHex: ")
  (message "%d" (string-to-number n 16)))

(defun int-to-eul-rep (n)
  (logior (lsh n 2) 1))

(defun eul-rep-to-int (n)
  (lsh n -2))

(tkb-keys
  ("\C-cH" 'tkb-dec-to-hex)
  ("\C-cD" 'tkb-hex-to-dec))


;; tempo templates
(require 'tempo)
(tempo-define-template
 "tkb-weblog"
 '(
   "<!DOCTYPE webpage PUBLIC \"-//Norman Walsh//DTD Website V2.6//EN\"
  \"http://docbook.sourceforge.net/release/website/2.6/schema/dtd/website.dtd\" [
<!ENTITY % logentities SYSTEM \"../../../log-entities.genent\">
%logentities;
<!ENTITY % myentities SYSTEM \"../../../myentitites.ent\">
%myentities;
]>\n\n"

   "<webpage id=\"" (setq tkb-weblog-id (format-time-string "log-%Y-%m-%d" tkb-weblog-time)) "\">
<config param=\"filename\" value=\"" (setq tkb-weblog-link (format-time-string "log/%Y/%m/%d" tkb-weblog-time)) ".html\"/>
<config param=\"rcsdate\" value=\"" (setq tkb-weblog-date (tkb-timestamp nil tkb-weblog-time)) "\"/>
<head>
<title>" tkb-weblog-date "</title>
<summary>" (p "Summary: " summary) "</summary>
</head>
<para>&" tkb-weblog-id "-next; &" tkb-weblog-id "-prev;</para>
<para>" p " </para>
</webpage>
"))


(defun tkb-weblog-old (time)
  (interactive "P")
  (let ((tkb-weblog-time (if time (tkb-get-date-from-user) nil)))
    (let ((weblog-dir (expand-file-name
		       (format-time-string
			"~/comp/website/source/log/%Y/%m/"
			tkb-weblog-time)))
	  (weblog (expand-file-name
		   (format-time-string
		    "~/comp/website/source/log/%Y/%m/%d.xml"
		    tkb-weblog-time))))
      (if (not (file-directory-p weblog-dir))
	  (make-directory weblog-dir 'parents))
      (if (file-exists-p weblog)
	  (find-file weblog)
	(find-file weblog)
	(tempo-template-tkb-weblog)
	(save-buffer)))))

(defun tkb-weblog (time)
  (interactive "P")
  (let ((tkb-weblog-time (if time (tkb-get-date-from-user) nil)))
    (let ((weblog-dir (expand-file-name
		       (format-time-string
			"~/comp/xsl-website/me/log/%Y/%m/"
			tkb-weblog-time)))
	  (weblog (expand-file-name
		   (format-time-string
		    "~/comp/xsl-website/me/log/%Y/%m/%d.xml"
		    tkb-weblog-time))))
      (if (not (file-directory-p weblog-dir))
	  (make-directory weblog-dir 'parents))
      (if (file-exists-p weblog)
	  (find-file weblog)
	(find-file weblog)
	(tempo-template-tkb-weblog)
	(save-buffer)))))


(defun tkb-kill-excess ()
  (interactive)
  (beginning-of-line)
  (while (not (eobp))
    (if (not (looking-at "^\\(.*\\)\\.zip"))
	(kill-line 1)
      (forward-line))))

(defun tkb-insert-rpg-net-post-url (post-id)
  (interactive "sPost Id: ")
  (insert (format "http://forum.rpg.net/showthread.php?s=&postid=%s#post%s"
		  post-id post-id)))

(defun tkb-get-x-selection ()
  (let (text)
    (when x-select-enable-clipboard
      (if (null text)
	  (condition-case c
	      (setq text (x-get-selection 'CLIPBOARD 'COMPOUND_TEXT))
	    (error nil)))
      (if (null text)
	  (condition-case c
	      (setq text (x-get-selection 'CLIPBOARD 'STRING))
	    (error nil)))
      (if (string= text "") (setq text nil)))

    ;; Don't die if x-get-selection signals an error.
    (if (null text)
	(condition-case c
	    (setq text (x-get-selection 'PRIMARY 'COMPOUND_TEXT))
	  (error nil)))
    (if (null text)
	(condition-case c
	    (setq text (x-get-selection 'PRIMARY 'STRING))
	  (error nil)))
    (if (string= text "") (setq text nil))

    (or text (setq text (x-get-cut-buffer 0)))
    (if (string= text "") (setq text nil))

    text))

(defun tkb-snarf-rpg-net-url ()
  (interactive)
  (let ((text (tkb-get-x-selection)))
    (message "text: %S" text)
    (when (and text (string-match "postid=\\([0-9]+\\)" text))
      (message "(match-string 1): %S" (match-string 1 text))
      (tkb-insert-rpg-net-post-url (match-string 1 text)))))

(defun tkb-insert-url-quoted (c)
  (interactive "cCharacter:")
  (insert (format "%%%x" c)))
(defun tkb-substitute-url-quoted ()
  (interactive)
  (let* ((cs (buffer-substring-no-properties (point) (+ (point) 1)))
	 (c (aref cs 0)))
    (kill-region (point) (+ (point) 1))
    (tkb-insert-url-quoted c)))

(defvar tkb-saved-buffer-name nil)
(defun tkb-save-buffer-name ()
  (interactive)
  (setq tkb-saved-buffer-name (buffer-file-name)))
(defun tkb-insert-saved-buffer-name ()
  (interactive)
  (insert tkb-saved-buffer-name))

(tkb-keys ("\C-cs" 'tkb-save-buffer-name))
(tkb-keys ("\C-cS" 'tkb-insert-saved-buffer-name))

(defun tkb-unwebify-region (p m)
  (interactive "r")
  (let ((s (buffer-substring-no-properties p m)))
    (message "%d %d s: %s" p m s)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (search-forward-regexp "%\\([0-9a-f][0-9a-f]\\)" nil t)
	(let ((v (match-string 1)))
	  (replace-match (char-to-string (string-to-number v 16)))))
      (copy-region-as-kill (point-min) (point-max)))))



(defun tkb-make-marker (&optional pos insertion-type)
  "Return a marker that points to position POS with insertion type
 INSERTION-TYPE."
  (unless pos (setq pos (point)))
  (let ((m (make-marker)))
    (set-marker m pos)
    (when insertion-type
      (set-marker-insertion-type m t)) ; marker stays after inserted text.
    m))

(defvar tkb-yank-place nil)

(defun tkb-mark-yank-place ()
  "Mark a specific place in a buffer to yank to later."
  (interactive)
  (tkb-clear-yank-place)
  (setq tkb-yank-place (tkb-make-marker nil t))
  (message "tkb-yank-place set"))

(defun tkb-clear-yank-place ()
  (when tkb-yank-place
    (set-marker tkb-yank-place nil)
    (setq tkb-yank-place nil)))

(defun tkb-yank-at-place ()
  "Yank at a specific place in a buffer."
  (interactive)
  (if (not tkb-yank-place)
      (message "No yank place set")
    (goto-char tkb-yank-place)
    (yank)))

(tkb-keys ("\C-\M-y" #'tkb-yank-at-place)
	  ("\C-c\C-@" #'tkb-mark-yank-place))

(defun tkb-vms-translate-keys ()
  "remap keys to C-S and C-Q to get around VMS flow
control nightmare.  Used elsewhere to ease fingermemory context
switch problems."
  (interactive)
  ;;Combat problems with C-s and C-q on VMS
  (when (eq system-type 'vax-vms)
    ;; Make emacs understand flow control; note that the versions of Emacs
    ;; I have on MPLVAX (VAX/VMS 5.5-2, Emacs 18.59?) and COBK (Alpha/VMS 7?,
    ;; Emacs 19.??) probably expect set-input-mode to take 2 parameters.
    (set-input-mode nil t nil))
  ;;Now make a translate table to translate the keyboard entered keys
  (let ((i 0)
	(the-table (make-string 128 0)))
    (while (< i 128)
      (aset the-table i i)
      (setq i (1+ i)))
    ;; Map C-/ to C-S (US); PuTTY thinks this is C-_
    (aset the-table ?\037 ?\C-s)
    ;; Map C-^ to C-Q; PuTTY gets this right
    (aset the-table ?\036 ?\C-q)
    ;; Map C-4 to C-Q (FS); PuTTY thinks this is C-\
    (aset the-table ?\034 ?\C-q)
    (setq keyboard-translate-table the-table)))


(defun tkb-quote-for-elisp (beg end)
  (interactive "r")
  (kill-new (format "%S" (buffer-substring-no-properties beg end))))


(defun tkb-w32-select-font ()
  ;; hacked from w32-win.el:(mouse-set-font &rest fonts)
  (if w32-use-w32-font-dialog
      (let ((chosen-font (w32-select-font (selected-frame)
					  w32-list-proportional-fonts)))
	(and chosen-font (list chosen-font)))
    (x-popup-menu
     last-nonmenu-event
     ;; Append list of fontsets currently defined.
     ;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
     (if (fboundp 'new-fontset)
	 (append w32-fixed-font-alist (list (generate-fontset-menu)))))))

(defun tkb-w32-copy-select-font (first-only)
  (interactive "P")
  (message "%S" first-only)
  (let* ((fonts (tkb-w32-select-font))
	 (fonts (if first-only (car fonts) fonts)))
    (kill-new (format "%S" fonts))))

(defun is-type (obj &rest typesyms)
  (memq (type-of obj) typesyms))

(defun tkb-edit-form (sym)
  (interactive (list (intern (completing-read "symbol? " obarray))))
  (let* ((value (eval sym))
	 (quote-string
	  (cond ((null value) nil)
		((is-type value 'integer 'float 'string) nil)
		((is-type value 'cons) 'quote))))
    ;; Emacs lisp needs a better pretty printer.
    (pp `(setq ,sym ,(if quote-string
			 `(,quote-string ,value)
		       `,value))
	(current-buffer))))

(message "End of tkb-experimental.el")
;;; end of tkb-experimental.el
