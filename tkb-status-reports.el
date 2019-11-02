;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-status-reports.el -- create and move around between status reports.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Variables
(defvar tkb-status-report-directory "~/job/MPL/StatusReports"
  "*Directory that contains ReStructuredText status reports and status report
templates")

(defvar tkb-status-report-template
  (expand-file-name "daily.rst.template" tkb-status-report-directory)
  "*File that contains the template for the daily status report.")



;;; Autoinsert stuff to support status reports

(progn 					; Probably should be elsewhere.
  (require 'autoinsert)
  (add-hook 'find-file-hooks 'auto-insert)
  (setq auto-insert-directory (concat (getenv "HOME") "/auto/")))

(defun tkb-auto-update-status-report-template ()
  (let ((time (cond ((string-match "status-\\([-0-9]+\\)\\.rst"
				   buffer-file-name)
		     (tkb-parse-iso-date (match-string 1 buffer-file-name)))
		    (t (current-time)))))
    (end-of-line)
    (insert (format-time-string "%Y-%2m-%2d" time))
    (insert " — ")	   ;format-time-string doesnt understand utf-8
    (insert (format-time-string "%A, %-e %B %Y" time))
    (end-of-line 2)
    (rst-repeat-last-character nil)))

(push `("status-.+\\.rst$" . [,tkb-status-report-template
			      tkb-auto-update-status-report-template])
      auto-insert-alist)



;;; Utilities, mostly for working with calendar.el-style dates.
(defalias 's2n 'string-to-number)
(defalias 'c:a2g 'calendar-gregorian-from-absolute)
(defalias 'c:g2a 'calendar-absolute-from-gregorian)

(defun c:subg (gregorian days)
  "Subtract DAYS from calendar.el GREGORIAN date."
  (c:a2g (- (c:g2a gregorian) days)))

(defun c:addg (gregorian days)
  "Add DAYS to calendar.el GREGORIAN date."
  (c:a2g (+ (c:g2a gregorian) days)))

(defun* cg2et ((month day year))
  "Convert calendar.el gregorian date (MONTH DAY YEAR) (as from calendar.el)
to emacs time (as from `current-time')"
  (encode-time 1 1 1 day month year))

(defun cgfts (format-string gregorian)
  "Use FORMAT-STRING to format calendar.el GREGORIAN date."
  (format-time-string time-string (cg2et gregorian)))



;;; Finally, the status report functions.

(defun tkb-status-report-filename (time)
  (let* ((iso (format-time-string "%Y-%2m-%2d" time))
	 (filename (expand-file-name (concat "status-" iso ".rst")
				     tkb-status-report-directory)))
    filename))

(when nil
  (defun tkb-status-report-filename (time)
    (let* ((iso (format-time-string "%Y/%2m/status-%Y-%2m-%2d.rst" time))
	   (filename (expand-file-name iso
				       tkb-status-report-directory)))
      filename)))

(defun tkb-status-report-find-file (time)
  (interactive (list (tkb-parse-iso-date (read-string "ISO Date: "))))
  (find-file (tkb-status-report-filename time)))

(defun tkb-status-report-today (prefix)
  (interactive "P")
  (let* ((now (cond ((consp prefix) (tkb-get-date-from-user))
		    (t (current-time)))))
    (tkb-status-report-find-file now)))

(defun tkb-status-report-insert-interval ()
  (interactive)
  (call-interactively #'tkb-status-report-today)
  (goto-char (point-min))
  (search-forward ".. \n   (")
  (up-list)
  (backward-char)
  (t:insert-interval))

(defun* ci2g (&optional time)
  (let ((year  (s2n (format-time-string "%Y" time)))
	(month (s2n (format-time-string "%m" time)))
	(day   (s2n (format-time-string "%d" time))))
    (list month day year)))

(defun tkb-status-report-filename-to-date (filename)
  "Match FILENAME and construct a calendar.el Gregorian date of the 
form (MONTH DAY YEAR)."
  (if (not (string-match "^status-\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)$" filename))
      (error "%s is not a status report file" buffer-file-name)
    (let ((gregorian (list (s2n (match-string 2 filename))
			   (s2n (match-string 3 filename))
			   (s2n (match-string 1 filename)))))
      gregorian)))

(defun tkb-date-for-display (&optional time)
  (format-time-string "%A, %e %B %Y (%Y-%m-%d)" time))

(defun tkb-status-report-find-previous-date (&optional n date)
  "Find the Nth previous status report."
  (interactive)
  (unless n (setq n 1))
  (cond ((eq date t) (setq date (ci2g nil)))
	(date date)
	(t (setq date (tkb-status-report-filename-to-date (t:bfnse)))))
  (loop with first-missing = nil
	with last-missing = nil
	for gregorian = (c:subg date n)
	then (c:subg gregorian n)
	for filename = (tkb-status-report-filename (cg2et gregorian))
	until (file-exists-p filename)
	do (progn
	     (unless first-missing (setq first-missing gregorian))
	     (setq last-missing gregorian)
	     (message "%s missing" filename)
	     (sit-for 0.1))
	finally do (when first-missing
		     (message "Missing %s to %s"
			      (tkb-date-for-display (cg2et first-missing))
			      (tkb-date-for-display (cg2et last-missing))))
	finally return (cg2et gregorian)))


(defun tkb-status-report-previous (n)
  (interactive "p")
  (tkb-status-report-find-file (tkb-status-report-find-previous-date n)))


(tkb-keys ("\C-cksf" #'tkb-status-report-find-file)
	  ("\C-ckst" #'tkb-status-report-today)
	  ("\C-cksp" #'tkb-status-report-previous)
	  ("\C-cksn" (lambda () (interactive) (tkb-status-report-previous -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;- Status Reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
