(unless (fboundp 'empty-string-p)
  (defun empty-string-p (s)
    "Return true if S is empty"
    (zerop (length s))))

(defun t:format-time (&optional time)
  (unless time (setq time (current-time)))
  (format-time-string "%I:%M %p" time))

(defun t:match-time (s)
  "Match a time string of the form 9a (9 am) or 12:30p and return it as a
decimal of the form 12.50.  12a is 0. 1p is 13.
WARNING: Doesn't handle seconds."
  (cond
   ((string-match (rx (group (1+ digit))   ; group 1: hour
                      (optional (group ":" ; group 2: :&minute
                                       (group (1+ digit)))) ; group 3: minute
		      (0+ " ")
                      (group (or "a" "p")) ; group 4: a/p
                      ) s)
    (let ((hour     (string-to-number (match-string 1 s)))
          (fraction (if (match-string 3 s)
                        (/ (string-to-number (match-string 3 s)) 60.0)
                      0.0))
          (ampm     (aref (match-string 4 s) 0)))
      (cond ((and (char-equal ampm ?p) (< hour 12))
             (incf hour 12))
            ((and (char-equal ampm ?a) (= 12 hour))
             (setq hour 0)))
      (+ hour fraction)))
   (t (error "%s is not a valid time string" s))))


(defun t:time-diff (start end)
  "Subtract START time from END time."
  (interactive "sStart: \nsEnd: ")
  (message "start: %s end: %s" start end)
  (let ((s (t:match-time start))
        (e (t:match-time end)))
    (when (= e 0) (setq e 24))
    (when (< e s)
      (error "start %s (%.2f) should be before end %s (%.2f) " start s  end e))
    (let ((d (- e s)))
      (message "s: %s (%.2f) e: %s (%.2f) Duration: %.2f hours"
	       start s end e d)
      d)))

(defun t:show-time-diff ()
  (interactive)
  (let* ((sexp  (sexp-at-point))
	 (start (car sexp))
	 (end   (cadr sexp)))
    (t:time-diff start end)))

(defun t:on-blank-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun t:on-2-blank-lines-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\n[ \t]*\n")))

(defun chomp (str)
  ;; http://emacswiki.org/emacs/ElispCookbook#toc6
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
		       str)
    (setq str (replace-match "" t t str)))
  str)

(defun t:total-intervals (intervals &optional print-intervals)
  "INTERVALS is a list of (START END . DESC) lists, where DESC is optional."
  (when print-intervals
    (when (not (t:on-2-blank-lines-p)) (princ "\n\n" print-intervals)))
  (when (/= (current-column) 0)
    (beginning-of-line))
  (let* ((tab (make-hash-table :test 'equal))
	 (total
	  (cl-loop for (a b . c)
		in (remove-if-not
		    #'(lambda (interval)
			(let* ((start-empty
				(empty-string-p (car interval)))
			       (end-empty
				(empty-string-p (cadr interval)))
			       (skipping (or start-empty end-empty)))
			  (when skipping
			    (message "skipping %S because %s empty"
				     interval
				     (if (and start-empty end-empty)
					 "start and end are"
				       (if start-empty
					   "start is"
					 "end is"))))
			  (not skipping)))
		    (mapcar #'(lambda (interval)
				(mapcar #'chomp interval))
			    intervals))
		sum (let* ((end (t:match-time b))
			   (beg (t:match-time a))
			   (dif (- end beg)))
		      (incf (gethash (if c (car c) nil) tab 0.0) dif)
		      (when print-intervals
			(princ (format "   %-6s to %-6s = %9f%s\n"
				       a b dif (if c (format " (%s)" (car c))
						 ""))
			       print-intervals))
		      dif))))
    (when print-intervals
      (princ (format "   %8s   %8s    %9s\n" "" "" "---------") print-intervals)
      (princ (format "   %8s   %8s    %9f total\n" "" "" total)
	     print-intervals)
      (princ "   ;\n" print-intervals)
      (cl-loop for key being the hash-keys of tab using (hash-values value)
	    sum value
	    do (princ (format "   %8s   %8s    %9f %s\n" "" "" value key)
		      print-intervals)))
    total))

(defun t:total-intervals-2 (intervals &optional insert-intervals quiet)
  "INTERVALS is a list of (START END . DESC) lists, where DESC is optional.
if END is a dash, use the current time instead."

  (cl-labels ((verbose (&rest args) (unless quiet (apply #'message args))))
    (let* (msgs
	   (tab (make-hash-table :test 'equal))
	   (total
	    (cl-loop for (beg* end* . comments)
		  in (remove-if-not
		      #'(lambda (interval)
			  (let* ((start-empty
				  (empty-string-p (car interval)))
				 (end-empty
				  (empty-string-p (cadr interval)))
				 (skipping (or start-empty end-empty)))
			    (when skipping
			      (verbose "skipping %S because %s empty"
				       interval
				       (if (and start-empty end-empty)
					   "start and end are"
					 (if start-empty
					     "start is"
					   "end is"))))
			    (not skipping)))
		      (mapcar #'(lambda (interval)
				  (mapcar #'chomp interval))
			      intervals))
		  sum (let* ((end (if (string-equal end* "-")
				      ;; Tricky: if the end is a dash, use the
				      ;; current time.
				      (t:match-time
				       (format-time-string "%I:%M%p"))
				    (t:match-time end*)))
			     (beg (t:match-time beg*))
			     (dif (- end beg)))
			(incf (gethash (if comments (car comments) nil)
				       tab 0.0) dif)
			(push (format "%-8s to %-8s = %9f%s\n"
				      beg* end* dif
				      (if comments
					  (format " (%s)" (car comments))
					""))
			      msgs)
			dif))))
      (push (format "%8s   %8s    %9s\n" "" "" "---------") msgs)
      (push (format "%8s   %8s    %9f total\n" "" "" total) msgs)
      (push (concat (make-string 70 ?=) "\n") msgs)
      (cl-loop for key being the hash-keys of tab using (hash-values value)
	    sum value
	    do (push (format "%8s   %8s    %9f %s\n" "" "" value key) msgs))
      (let ((msg (apply #'concat (mapcar #'(lambda (s) (concat "    " s))
					 (reverse msgs)))))
	(verbose"%s" msg)
	(when insert-intervals
	  (insert "\n\n" msg)
	  (verbose "inserted?")))
      total)))

(defun t:show-total-intervals (insert-intervals)
  (interactive "P")
  (message "prefix: %S current-buffer: %S" insert-intervals (current-buffer))
  (let* ((time-intervals (sexp-at-point)))
    (t:total-intervals-2 time-intervals insert-intervals)))


(defun t:time-remaining (total-needed total-worked start)
  (interactive "nTotal Needed: \nnTotal Worked: \nsStart: ")
  (let* ((s (t:match-time start))
         (delta (- total-needed total-worked))
         (e (+ s delta))
         (h (truncate e))
         (.m (- e h))
         (m (round (* .m 60))))
    (destructuring-bind (sec min hour day month year dow dst zone)
	(decode-time (current-time))
      (let ((time (encode-time 0 m h day month year zone)))
	(message "%s = %s (delta: %f)"
		 ;; For some reason, "(encode-time 0 m h 0 0 0)" stopped working
		 ;; around GNU Emacs 23.4.1.
		 (format-time-string "%H:%M" time)
		 (format-time-string "%I:%M %p" time)
		 delta)))))

(defun t:time-remaining2 (total-needed start)
  (interactive "nTotal Needed: \nsStart: ")
  (let (intervals beg end (i 0))
    (catch 'done
      (while t
	(setq beg (read-string (format "Start %d: " i)))
	(when (empty-string-p beg) (throw 'done nil))
	(setq end (read-string (format "End   %d: " i)))
	(when (empty-string-p end) (throw 'done nil))
	(push (list beg end) intervals)
	(incf i)))
    (t:time-remaining total-needed (t:total-intervals intervals)  start)))

(defun t:time-remaining2 (hours-needed intervals start)
  "Total the hours in INTERVALS, subtract from HOURS-NEEDED, add to the START
time, to get the time to be finished."
  (interactive (list (read-number "Total needed: ")
		     (reverse
		      (let (intervals beg end (i 0))
			(catch 'done
			  (while t
			    (setq beg (read-string (format "Start %d: " i)))
			    (when (empty-string-p beg) (throw 'done intervals))
			    (setq end (read-string (format "End   %d: " i)))
			    (when (empty-string-p end) (throw 'done intervals))
			    (push (list beg end) intervals)
			    (incf i)))))
		     (read-string "Last Start: ")))
  (t:time-remaining hours-needed (t:total-intervals intervals) start))

(when nil
  (t:time-remaining
   40 
   (t:total-intervals `(("8:30a" "6:30p")  ;Monday
                        ("9:30a" "8:06p")  ;Tuesday
                        ("9:30a" "6:18p")  ;Wednesday
                        ("7:30p" "9:30p")  ;  at MPL
                        ("8:54a" "10:06a") ;Thursday, part I
                        ))
   "10:42a")
  ;; "18:06 = 06:06 PM (delta: 7.400000)"
  (t:time-remaining 8 (t:total-intervals '(("9:12a" "1:18p"))) "2:18p")
  ;; "18:12 = 06:12 PM (delta: 3.900000)"
  )

(defconst t:time-codes
  '(
    ("GOI"	 "Kay Atman's GOI web prototype")
    ("GOIApp"    "GOI Mobile App work with S. Reger")
    ("MHST"	 "Miners Health Saftey an Training")
    ("MHSTBill"  "Billable Miners Health Saftey an Training")
    ("MiscGnA"	 "Miscellaneous")
    ("PrattBill" "Pratt Billable")
    ("PrattNB"   "Pratt Non-billable")
    ("PrattEI"   "Pratt EI DB Upgrade")
    ("PrattRPD"  "Pratt Repairline Passdown")
    ("PrattStdCont"  "Pratt Standard Work Project Continuation")
    ("SISMigration"  "SIS Migration work (\"4.1.7 New System Migration\")")
    ("Staff"	 "Staff Meetings")
    ("SysAdmin"  "Corp System Administration")
    ("TASC-MP"   "TASC Mentor Protoge")
    ("VAXCon"	 "VAX Conversion")
    ("WVHTCFBill" "WVHTCF Billable")
    ("Breck"     "Breckenridge")
    ))

(defvar t:old-time nil)
(defvar t:time-history nil)

(defun t:insert-interval ()
  (interactive)
  (let* ((now (current-time))
	 (now*  (t:format-time now))
	 (start (upcase (read-string "Start Time? " now* 't:time-history now*)))
	 (end   (upcase (read-string "End Time?   "
				     t:old-time 't:time-history t:old-time)))
	 (code  (completing-read "Time Code? " t:time-codes))
	 (desc  (read-string "Description? ")))
    (insert (format "(%-10S %-10S %-12S %S)\n    "
		    start end code desc))))
(tkb-keys ((kbd "C-c k s i") 't:insert-interval))
		     
