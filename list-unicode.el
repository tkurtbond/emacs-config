(defmacro with-break (&rest body)
  (declare (indent 0)
	   (debug (&rest form)))
  `(progn
     ,@body
     (incf n)
     (if (= (% n 4) 0) (insert "\n"))
     (if (= (% n (expt 4 4)) 0) (redisplay))))


(defun list-unicode-characters ()
  "List all the unicode characters that Emacs supports."
  (interactive)
  (with-work-buffer " *Unicode Characters* "
    (loop for i from 0 to #x10FFFF
	  with n = 0
	  with skip = nil
	  do
	  (progn
	    (condition-case e
		(let ((c (tkb-ucs-check i)))
		  (when skip
		    (with-break 
		      (insert (format "  %6s: " "skip"))
		      (insert (format "%d" skip))
		      (insert ?\t))
		    (setq skip nil))
		  (with-break 
		    (insert (format "U+%06X: " i))
		    (let ((s (format "%s" (format "%c" c))))
		      (when (or (< c 32) (= c 127))
			;; Display control characters as ^ctrlchar,
			;; even \n and \t.
			(setq s (propertize s 'display
					    (text-char-description c))))
		      (insert s)
		      (insert ?\t))))
	      (ucs-error
	       (if skip
		   (incf skip)
		 (setq skip 1))))
	    (message "skip: %s i: %d n: %d" skip i n)))))









