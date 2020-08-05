;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-time.el -- Time options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-time.el 1.1 Sun, 26 Mar 2000 15:10:50 -0500 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calendar
(setq calendar-week-start-day 1)
(setq calendar-longitude -80.1627)
(setq calendar-latitude 39.458)
(setq cal-tex-diary t)

(setq display-time-string-forms
      '(12-hours ":" minutes " " (downcase am-pm) (if mail " Mail") load))

(defun string-or-nil-to-int (s)
  (if s
      (string-to-number s)
    0))

(defun tkb-parse-iso-date (date)
  "Parse DATE as an ISO format date (and optionally time) string
and return the encoded time."
  (cond ((string-match			;YYYY-MM-DD
	  "\\([0-9]\\{4\\}\\)[-/]\\([0-1][0-9]\\)[-/]\\([0-3][0-9]\\)\\( \\([0-2][0-9]\\)\\(\:\\([0-6][0-9]\\)\\)?\\(\:\\([0-6][0-9]\\)\\)?\\)?$"
	  date)
	 (let ((year (string-to-number (match-string 1 date)))
	       (mon  (string-to-number (match-string 2 date)))
	       (dom  (string-to-number (match-string 3 date)))
	       (hour (string-or-nil-to-int (match-string 5 date)))
	       (min  (string-or-nil-to-int (match-string 7 date)))
	       (sec  (string-or-nil-to-int (match-string 9 date))))
	   (encode-time sec min hour dom mon year)))
	((string-match			;MM-DD-YYYY
	  "\\([0-1][0-9]\\)[-/]\\([0-3][0-9]\\)[-/]\\([0-9]\\{4\\}\\)\\( \\([0-2][0-9]\\)\\(\:\\([0-6][0-9]\\)\\)?\\(\:\\([0-6][0-9]\\)\\)?\\)?$"
	  date)
	 (let ((year (string-to-number (match-string 3 date)))
	       (mon  (string-to-number (match-string 1 date)))
	       (dom  (string-to-number (match-string 2 date)))
	       (hour (string-or-nil-to-int (match-string 5 date)))
	       (min  (string-or-nil-to-int (match-string 7 date)))
	       (sec  (string-or-nil-to-int (match-string 9 date))))
	   (encode-time sec min hour dom mon year)))
	  
	(t
	 (error "invalid date: %s" date))))

(defun tkb-get-date-from-user (&optional default-time use-time)
  ;; Warning: doesn't validate date!
  (let* ((default-time (if default-time default-time (current-time)))
	 (date  (read-from-minibuffer
		"Date: " (format-time-string (if use-time "%Y-%m-%d %H:%M:%S" "%Y-%m-%d") default-time))))
    (tkb-parse-iso-date date)))

(defun tkb-parse-usa-date (d)
  ;; Warning: doesn't validate date!
  (if (not (string-match
	    "\\([0-1][0-9]\\)[-/]\\([0-3][0-9]\\)[-/]\\([0-9]\\{4\\}\\)"
	    d))
      (error "invalid date: %s" d)
    (let ((year (string-to-number (match-string 3 d)))
	  (mon  (string-to-number (match-string 1 d)))
	  (dom  (string-to-number (match-string 2 d))))
      (encode-time 0 0 0 dom mon year))))

(defun tkb-insert-date (prefix)
  "Insert a traditional date."
  (interactive "P")
  (let ((time (if prefix (tkb-get-date-from-user) (current-time))))
    (insert (tkb-timestamp nil time))))

(defun tkb-kill-date (prefix)
  "Copy a traditional date to the kill ring."
  (interactive "P")
  (let ((time (if prefix (tkb-get-date-from-user) (current-time))))
    (kill-new (tkb-timestamp nil time))))

(defun get-ampm (&optional time)
  (interactive)
  (let ((ampm (downcase (substring (format-time-string "%p" time) 0 1))))
    (let ((c (aref ampm 0)))
      (assert (member c '(?a ?p))
	      t "get-ampm: '%c' is not 'a' or 'p' - is locale weird?" c)
      ampm)))

(defun tkb-time ()
  "Return the current time, in \"HH:MM[ap]\" format."
  (let* ((now (current-time))
	 (ampm (get-ampm now)))
    (format "%s%s" (format-time-string "%I:%M") ampm)))

(defun tkb-insert-time ()
  "Insert the current time, in \"HH:MM[ap]\" format."
  (interactive)
  (insert (tkb-time)))



(defun date ()
  (interactive)
  (message "Date: %s" (tkb-timestamp)))

(defun tkb-insert-fancy-date (prefix)
  "Inserts a fancy date like this: 
Saturday, 4 July 2020, 01:47:15 AM (2020-07-04 01:47:15)."
  (interactive "P")
  (let ((time (if (null prefix)
                  (current-time)
                (tkb-get-date-from-user nil t))))
  (insert (format-time-string 
           "%A, %-d %B %Y, %I:%M:%S %p (%Y-%m-%d %H:%M:%S %Z)"))))
(global-set-key (kbd "C-c d f") 'tkb-insert-fancy-date)
(tkb-key-is-bound-to (kbd "C-c d f") 'tkb-insert-fancy-date)

(defun tkb-insert-iso-date (prefix)
  "Insert an ISO format date.
'C-u' prompts for date.
'C-u -' gets YYYY-MM-DD Weekday
'C-u NUM' adds or subtracts NUM from the date.
'C-u - C-u' adds the time.
"
  (interactive "P")
  (let ((time (cond
	       ((null prefix)		;No prefix specified
		(current-time))
	       ((and (consp prefix)
		     (>= (car prefix) 0))		;Just the default prefix
		(tkb-get-date-from-user))
	       ((integerp prefix)	;Prefix is delta in days
		(tkb-get-date-from-user
		 (time-add (current-time)
			   (seconds-to-time (* 24 60 60 prefix)))))
	       (t (current-time)))))
    (insert (format "%s%s"
		    (format-time-string (if (and (symbolp prefix)
						 (eq prefix '-))
					    "%Y-%2m-%2d %A"
					  "%Y-%2m-%2d") time)
		    (if (and (consp prefix)
			     (< (car prefix) 0))
			(format-time-string " %H:%M:%S" time) "")))))


(defun tkb-kill-iso-date (prefix)
  "Copy an ISO format date to the kill ring.
'C-u' prompts for date.
'C-u -' gets YYYY-MM-DD Weekday
'C-u NUM' adds or subtracts NUM from the date.
'C-u - C-u' adds the time."
  (interactive "P")
  (let* ((time (cond
		((null prefix)		;No prefix specified
		 (current-time))
		(prefix
		 ;; Condition used to be: 
		 (and (consp prefix)
		      (>= (car prefix) 0)) ;Just the default prefix
		 (tkb-get-date-from-user))
		((integerp prefix)	;Prefix is delta in days
		 (tkb-get-date-from-user
		  (time-add (current-time)
			    (seconds-to-time (* 24 60 60 prefix)))))
		(t (current-time))))
	 (s (format "%s%s"
		    (format-time-string (if (and (symbolp prefix)
						 (eq prefix '-))
					    "%Y-%2m-%2d %A"
					  "%Y-%2m-%2d") time)
		    (if (and (consp prefix)
			     (< (car prefix) 0))
			(format-time-string " %H:%M:%S" time) ""))))
    (kill-new s)
    (message "Killed ISO Date: %s" s)))


(defun tkb-insert-timestamp (&optional use-time)
  "Insert a timestamp to my liking."
  (interactive "P")
  (insert (tkb-timestamp use-time)))	;Did have "Last modified: "

(defvar tkb-timestamp-format-regexp
  (concat "\\(monday\\|tuesday\\|wednesday\\|"
	  "thursday\\|friday\\|staturday\\|sunday\\)" ;the days
	  ",[ \t]+"
	  "[0-9]+"			;the day of the month
	  "[ \t]+"
	  "\\(january\\|february\\|march\\|april\\|may\\|june\\|july\\|"
	  "august\\|september\\|october\\|november\\|december\\)"
	  "[ \t]+"
	  "[0-9]+"
  "Regular expression that matches my prefered timestamp format."))

(defun tkb-replace-timestamp ()
  "Replace a timestamp if the text following point matches my preferred
timestamp format"
  (interactive)
  (cond ((looking-at tkb-timestamp-format-regexp)
	 (let* ((beg (match-beginning 0))
		(end (match-end 0))
		(origin (copy-marker beg nil)))
	 (delete-region beg end)
	 (tkb-insert-timestamp)
	 (goto-char origin)))))

(defun tkb-timestamp (&optional use-time time)
  "Return a timestamp to my liking.  Append clock time if USE-TIME is true.
If TIME is true, use it for the time to format."
  (let* ((time      (if time time (current-time)))
      (hhmm         (format-time-string "%I:%M %p" time))
      (dow          (format-time-string "%A" time))
      (day          (int-to-string (string-to-number
                         (format-time-string "%e" time)))) ;fixme: %-e
      (month   (format-time-string "%B" time))
      (year         (format-time-string "%Y" time))
      (result       (concat dow ", " day " " month " " year))
      )
    (if use-time
     (concat result " at " hhmm)
      result)))


;; diary
;; this causes the diary never to display, for some reason.
;(add-hook 'diary-display-hook 'appt-make-list)

(defconst tkb-date-regexp "\\(\\([A-Za-z]+\\), *\\([0-9]+\\) *\\([A-Za-z]+\\) *\\([0-9]+\\).*\\)")

(defconst tkb-month-names
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
(defun tkb-date-month-number (name)
  (let ((n (position name tkb-month-names :test (function string-equal))))
    (if n
	(1+ n)
      n)))
(defun tkb-number-to-month-name (number)
  (aref tkb-month-names (1- number)))

(defun tkb-months ()
  "Display month numbers and abbreviations in a buffer."
  (interactive)
  (let ((b (get-buffer-create "*tkb months*")))
    (with-current-buffer b
      (erase-buffer)
      (loop for month across tkb-month-names
	    for i from 1
	    do (insert (format "%2d: %s\n" i month)))
      (fit-window-to-buffer (display-buffer b)))))

;;; end of tkb-time.el
