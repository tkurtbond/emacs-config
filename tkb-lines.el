(defun current-line ()
  "Return the current buffer line number."
  (interactive)
  (let ((opoint (point)) start)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(widen)
	(forward-line 0)
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(1+ (count-lines 1 (point)))))))
