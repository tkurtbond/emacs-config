;;;; tkb-duration.el -- Calculate duration from time interval s.

(defconst tkb-time-rx
  ;; Regular expression groups:
  ;; 1                   2   3                     4
  "\\([0-9]\\{1,2\\}\\)\\(:\\([0-9]\\{2\\}\\)\\)?\\([ap]m?\\)?"
  "Matches a time of day, like 1:30 or 2p or 11:30a")
(defconst tkb-to-rx "\\s-+to\\s-+")
(defconst tkb-time-interval-rx (concat tkb-time-rx tkb-to-rx tkb-time-rx)
  "Matches two times of day, separated by the word \"to\".")

(defconst tkb-duration-with-or-rx
  "\\([0-9]+\\(:[0-9]+\\)?\\)[ \t]+or[ \t]+\\([0-9]+\\(\\.[0-9]+\\)?\\)[ \t]+hours"
  "Matches a duration in form \"h:m or h.d hours\", where h is hours, 
m is minutes, and d is decimal fractional hours.")
  


(defun tkb-s-to-n (s)
  "Convert N to a number, with nil converted to 0."
  (if s
      (string-to-number s)
    0))


(defun tkb-m-to-h (d)
  "Convert a duration in minutes to a string containing decimal hours."
  (format "%0.2f" (/ d 60.0)))


(defun tkb-m-to-h-mm  (d)
  "Convert a duration in minutes to a string contain hours and minutes 
in the form h-mm."
  (let ((h (/ d 60))
	(m (% d 60)))
    (format "%d:%02d" h m)))

(defun tkb-h-to-h-mm (d)
  "Convert a duraction in decimal hours to a string containing hours and minutes"
  (let* ((h (truncate d))
         (m (truncate (* (- d h) 60)))
         (s (format "%d:%02d" h m)))
    s))

(defun tkb-backward-number ()
  (with-syntax-table emacs-lisp-mode-syntax-table
    (backward-sexp)))

(defun tkb-eval-last-number ()
  (save-excursion
    (let ((e (point))
          b)
      (tkb-backward-number)
      (setq b (point))
      (string-to-number (buffer-substring b e)))))

(defun tkb-show-h-mm-of-last-duration ()
  (interactive)
  (message "=> %s" (tkb-h-to-h-mm (tkb-eval-last-number))))

(defun tkb-insert-h-mm-of-last-duration ()
  (interactive)
  (insert (tkb-h-to-h-mm (tkb-eval-last-number))))

(defun tkb-is-am-p (ampm)
  (or (= (aref ampm 0) ?A)
      (= (aref ampm 0) ?a)))

(test (tkb-is-am-p "a") t)
(test (tkb-is-am-p "A") t)
(test (tkb-is-am-p "am") t)
(test (tkb-is-am-p "AM") t)

(defun tkb-is-pm-p (ampm)
  (or (= (aref ampm 0) ?P)
      (= (aref ampm 0) ?p)))
	   
(test (tkb-is-pm-p "p") t)
(test (tkb-is-pm-p "P") t)
(test (tkb-is-pm-p "pm") t)
(test (tkb-is-pm-p "PM") t)

(defun tkb-to-24-hour (h ampm)
  ;; See http://en.wikipedia.org/wiki/12-hour_clock#Confusion_at_noon_and_midnight

  (cond ((and (= h 12) (tkb-is-am-p ampm))
	 ;; ANSI standard says 12am is midnight, so 00:00 in 24 hour time.
	 0)
	((and (= h 12) (tkb-is-pm-p ampm))
	 ;; ANSI standard says 12pm is noon, so 12:00 in 24 hour time.
	 12)
	((and (<= h 12) (tkb-is-pm-p ampm))
	 ;; It is in the pm, so adding 12 converts it to 24 hour time.
	 (+ h 12))
	(t h)))

(defun tkb-get-duration (&optional s)
  "Calculate duration, returning a list with an integer and two
strings: the duration in minutes, the duration as hours and
minutes (h-mm), and the duration as decimal hours (h.hh).

The optional argument S is the original string if the matching
was done by string-match, for non-interactive use.

Warning: Call this only after having matched tkb-time-interval-rx."
  (interactive)
  (let ((beg-hour (tkb-s-to-n (match-string 1 s)))
	(beg-min  (tkb-s-to-n (match-string 3 s)))
	(beg-ampm             (match-string 4 s))
	(end-hour (tkb-s-to-n (match-string 5 s)))
	(end-min  (tkb-s-to-n (match-string 7 s)))
	(end-ampm             (match-string 8 s))
	(errors nil))

    ;; Don't allow ampm with 24 hour clock times.
    (when (and beg-ampm (> beg-hour 12))
      (push (format "am or pm forbidden for 24 hour clock times %s%s."
		    beg-hour beg-ampm) errors))
    (when (and end-ampm (> end-hour 12))
      (push (format "am or pm forbidden for 24 hour clock times %s%s."
		    end-hour end-ampm) errors))

    ;; Don't allow nil ampm with 12 hour clock times.
    (when (and (not beg-ampm) (<= beg-hour 12))
      (push (format "am or pm required for 12 hour clock times: %s."
		    beg-hour)
	    errors))
    (when (and (not end-ampm) (<= end-hour 12))
      (push (format "am or pm required for 12 hour clock times: %s."
		    end-hour)
	    errors))

    (when errors
      (error "%s" (mapconcat #'identity errors "\n")))
    
    ;; Convert to 24 hour clock times.
    (setq beg-hour (tkb-to-24-hour beg-hour beg-ampm))
    (setq end-hour (tkb-to-24-hour end-hour end-ampm))
	  
    (let* ((beg  (+ (* beg-hour 60) beg-min))
	   (end  (+ (* end-hour 60) end-min)))
      ;; FIXME: Does this handle 11:30p to 1a???
      (when (<= end beg) (incf end (* 24 60)))
      (let* ((dur  (- end beg))
	     (h-mm (tkb-m-to-h-mm dur))
	     (h    (tkb-m-to-h dur)))
	(tkb-get-duration-print-variables)
	(list dur h-mm h)))))

(defun tkb-get-duration-print-variables ()
  "Useful only for debugging 'tkb-get-duration'."
  ;; This only works because emacs lisp uses dynamic scope.
  (message (concat "bh: %s bmp: %s bap: %s "
		   "eh: %s emp: %s eap: %s "
		   "dur: %s h-mm: %s h: %s")
	   beg-hour beg-min beg-ampm
	   end-hour end-min end-ampm
	   dur h-mm h))


(defun tkb-interval-to-string (interval)
  (let ((case-fold-search t))
    (when (string-match (concat "\\`" tkb-time-interval-rx "\\'") interval)
      (tkb-get-duration interval))))


(defun tkb-display-duration ()
  "If looking at a time interval, display the duration of the interval."
  (interactive)
  (if (not (looking-at tkb-time-interval-rx))
      (message "No time interval here.")
    (destructuring-bind (duration h-mm hours) (tkb-get-duration)
      (message "%s or %s hours" h-mm hours))))


(defun tkb-insert-duration ()
  "Extract a duration from a time interval on the current line,
display it, and optionally insert it at the beginning of the
line."
  (interactive)
  (let (e p)
    (if (save-excursion
	  (end-of-line)
	  (setq e (point))
	  (beginning-of-line)
	  (not (search-forward-regexp tkb-time-interval-rx e nil)))
	(message "No time interval on this line.")
      (destructuring-bind (duration h-mm hours) (tkb-get-duration)
	(let* ((formatted-duration (format "%s or %s hours" h-mm hours)))
	  (save-excursion
	    (beginning-of-line)
	    (if (search-forward-regexp tkb-duration-with-or-rx e t)
		(progn 
		  (setq p (match-beginning 0))
		  (cond ((string-equal (match-string 0) formatted-duration)
			 (message "Duration is same, not replacing %s"
				  (match-string 0)))
			((y-or-n-p (format "Replace %s with %s? "
					   (match-string 0)
					   formatted-duration))
			 (replace-match formatted-duration t t))))
	      (when (y-or-n-p (format "Duration: %s; insert? "
				      formatted-duration))
		(end-of-line)
		(unless (save-excursion
			  (backward-char)
			  (looking-at " "))
		  (insert " "))
		(setq p (point))
		(insert formatted-duration))))
	  (when p (goto-char p)))))))
(tkb-keys ("\C-cj" 'tkb-insert-duration))
  

;; Test the regular expression matching.
(test (tkb-interval-to-string "111 to 2") nil)
(test (tkb-interval-to-string "") nil)
(test (tkb-interval-to-string "1a to 2p") (780 "13:00" "13.00"))
(test (tkb-interval-to-string "1am to 2pm") (780 "13:00" "13.00"))
(test (tkb-interval-to-string "2pm to 1am") (660 "11:00" "11.00"))

;; Don't allow am or pm with 24 hour clock times.
(test (tkb-interval-to-string "13a to 23p") nil error)


;; Test the duration calculation, with explicit ampm markers.
(test (tkb-interval-to-string "1a to 2a") (60 "1:00" "1.00"))
(test (tkb-interval-to-string "11a to 1:15p") (135 "2:15" "2.25"))
(test (tkb-interval-to-string "8:15a to 8:15p") (720 "12:00" "12.00"))
(test (tkb-interval-to-string "12a to 12p") (720 "12:00" "12.00"))
(test (tkb-interval-to-string "1a to 1a")  (1440 "24:00" "24.00"))
(test (tkb-interval-to-string "1p to 1p")  (1440 "24:00" "24.00"))
(test (tkb-interval-to-string "1p to 1a") (720 "12:00" "12.00"))
(test (tkb-interval-to-string "2a to 1p") (660 "11:00" "11.00"))
(test (tkb-interval-to-string "12p to 12a") (720 "12:00" "12.00"))

;; Don't allow 24 hour clock times with am or pm.
(test (tkb-interval-to-string "13a to 24p") (1440 "24:00" "24.00") error)



;; These cases don't work yet or are not yet explictly handled.

;; If neither ampm specified and t1 < t2, assume both are same ampm.
(test (tkb-interval-to-string "1 to 2") (60 "1:00" "1.00"))
;; If t1 is am and t1 < t2, assume t2 is am as well.
(test (tkb-interval-to-string "1a to 2") (60 "1:00" "1.00"))
;; if no ampm for t1 and t1 < t2 and t2 is am, assume t1 is am
(test (tkb-interval-to-string "1 to 2a") (60 "1:00" "1.00"))
(test (tkb-interval-to-string "1 to 13") (720 "12:00" "12.00"))
(test (tkb-interval-to-string "1 to 12") (660 "11:00" "11.00"))
;; if no ampm ofr t1 and t1 < t2 and t2 is pm, assume t1 is am?
(test (tkb-interval-to-string "1 to 2p") (60 "1:00" "1.00"))
;; if t1 is pm and 
(test (tkb-interval-to-string "1p to 2") (60 "1:00" "1.00"))

(test (tkb-interval-to-string "13 to 14") (60 "1:00" "1.00"))
(test (tkb-interval-to-string "14 to 13") (1380 "23:00" "23.00"))
(test (tkb-interval-to-string "1 to 14") (780 "13:00" "13.00"))
(test (tkb-interval-to-string "14 to 1") (660 "11:00" "11.00"))
(test (tkb-interval-to-string "1 to 23") (1320 "22:00" "22.00"))
(test (tkb-interval-to-string "23 to 1") (120 "2:00" "2.00"))


(test (tkb-interval-to-string "2 to 1") (660 "11:00" "11.00"))
(test (tkb-interval-to-string "2a to 1") (660 "11:00" "11.00"))
(test (tkb-interval-to-string "2 to 1a") (660 "11:00" "11.00"))
(test (tkb-interval-to-string "2 to 1p") (660 "11:00" "11.00"))
(test (tkb-interval-to-string "2p to 1") (660 "11:00" "11.00"))

(test (tkb-interval-to-string "12 to 12") (720 "12:00" "12.00"))
(test (tkb-interval-to-string "8:15 to 8:15") (720 "12:00" "12.00"))

(test (tkb-interval-to-string "04:14PM to 10:59AM")(1125 "18:45" "18.75"))
(test (tkb-interval-to-string "04:14pm to 10:59am") (1125 "18:45" "18.75"))

(test (tkb-interval-to-string "10:15a to 12:15p") (120 "2:00" "2.00"))
(test (tkb-interval-to-string "10a to 12a") (120 "2:00" "2.00"))

;;; end of tkb-duration.el
