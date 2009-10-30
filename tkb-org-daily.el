(defvar org-daily-directory "~/current/org/daily")
(defvar org-daily-template "~/current/org/daily/daily.template")
(defvar org-daily-day-template "~/current/org/daily/day.template")
(defvar org-daily-subject-history '())
(defvar org-daily-code-history '())
(defvar org-daily-time-history '())
(defvar org-daily-today-position nil)

(defun org-daily-directory-create ()
  (unless (file-exists-p org-daily-directory)
    (if (not (y-or-n-p (format "org-daily directory %s missing. Create it? "
			       org-daily-directory)))
	(error "Org-Daily directory %s missing and not created" org-daily-directory)
      (make-directory org-daily-directory t))))

(defun org-daily-find-file ()
  (let* ((org-daily-file (expand-file-name
			  (format "%s%s" (format-time-string "%Y-%m") ".org")
			  org-daily-directory)))
    (find-file org-daily-file)
    (unless (file-exists-p org-daily-file)
      (insert-file-contents org-daily-template))))

(defun org-daily-add-day-entry ()
  (goto-char (point-min))
  (let* ((org-daily-date (format-time-string "* work time <%Y-%m-%d %a>")))
    (cond ((re-search-forward (concat "^" org-daily-date "$")
			      nil 'move-to-limit)
	   (setq org-daily-today-position (point)))
	  (t
	   (insert "\n" org-daily-date "\n")
	   (insert-file org-daily-day-template)))))

(defun org-daily (&optional time)
  "Add a org-daily journal entry.  With a prefix arg, add a time entry."
  (interactive "P")
  (org-daily-directory-create)
  (org-daily-find-file)
  (org-daily-add-day-entry)
  (when time
    (let* ((time-string (read-string "Time: "
				     (format-time-string "%H:%M") nil))
	   (subject (read-string "Subject: " nil 'org-daily-subject-history))
	   (string (if (empty-string-p subject)
		       time-string
		     (concat time-string ": " subject)))
	   (underline (make-string (length string)
				   org-daily-time-entry-underline)))
      (unless (looking-at "^$") (insert "\n"))
      (insert "\n" string "\n" underline "\n\n"))))


