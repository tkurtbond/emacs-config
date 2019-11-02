(defvar daily-directory "~/current/daily")
(defvar daily-template "~/current/daily/daily.template")
(defvar daily-subject-history '())
(defvar daily-code-history '())
(defvar daily-time-history '())
(defvar daily-day-entry-underline ?=)
(defvar daily-time-entry-underline ?-)
(defvar daily-today-position nil)

(defun daily-backup-to-entry ()
  (while (or (eobp) (looking-at "^\\.\\."))
    (forward-line -1))
  (unless (looking-at "^$")
    (end-of-line)))

(defun daily-directory-create ()
  (unless (file-exists-p daily-directory)
    (if (not (y-or-n-p (format "daily directory %s missing. Create it? "
			       daily-directory)))
	(error "Daily directory %s missing and not created" daily-directory)
      (make-directory daily-directory t))))

(defun daily-find-file ()
  (let* ((daily-file (expand-file-name
		      (format "%s%s" (format-time-string "%Y-%m") ".rst")
		      daily-directory)))
    (find-file daily-file)
    (unless (file-exists-p daily-file)
      (insert-file-contents daily-template))))

(defun daily-add-day-entry ()
  (goto-char (point-min))
  (let* ((daily-date (format-time-string "%F: %A, %-e %B %Y"))
	 (daily-underline (make-string (length daily-date)
				       daily-day-entry-underline)))
    (cond ((re-search-forward (concat "^" daily-date "\n" daily-underline "$")
			      nil 'move-to-limit)
	   (setq daily-today-position (point))
	   (goto-char (point-max))
	   (daily-backup-to-entry))
	  (t
	   (daily-backup-to-entry)
	   (insert "\n\n" daily-date "\n" daily-underline "\n")
	   (setq daily-today-position (point))
	   (insert "\n")))))

(defun daily (&optional time)
  "Add a daily journal entry.  With a prefix arg, add a time entry."
  (interactive "P")
  (daily-directory-create)
  (daily-find-file)
  (daily-add-day-entry)
  (when time
    (let* ((time-string (read-string "Time: "
				     (format-time-string "%H:%M") nil))
	   (subject (read-string "Subject: " nil 'daily-subject-history))
	   (string (if (empty-string-p subject)
		       time-string
		     (concat time-string ": " subject)))
	   (underline (make-string (length string)
				   daily-time-entry-underline)))
      (unless (looking-at "^$") (insert "\n"))
      (insert "\n" string "\n" underline "\n\n"))))

(setq daily-time-range-regexp
  (concat "^\\([0-9]+:[0-9]+\\)\\(am\\|\\pm\\) \\(to \\):.*\n"
	  (make-string 5 daily-time-entry-underline)))

(defun daily-add-time-range-entry ()
  (interactive)
  (daily-directory-create)
  (daily-find-file)
  (daily-add-day-entry)
  (when (re-search-backward daily-time-range-regexp daily-today-position t)
    (goto-char (match-end 3))
    (setq end-time
	  (read-string "End Time: " (format-time-string "%-I:%M%#p") nil))
    (insert end-time)
    (forward-line)
    (if (looking-at (make-string 1 daily-time-entry-underline))
	(rst-repeat-last-character)))
  (daily-add-day-entry)
  (let* ((start-time (read-string "Start Time: "
				  (if end-time end-time (format-time-string "%-I:%M%#p"))
				  'daily-time-history))
	 (code (read-string "Code: " nil 'daily-code-history))
	 (entry (format "%s to : %s (0:00 or 0.00 hours)" start-time code))
	 (entry-length (length entry))
	 (entry-underline (make-string entry-length
				       daily-time-entry-underline)))
    (insert entry "\n" entry-underline "\n\n")))
