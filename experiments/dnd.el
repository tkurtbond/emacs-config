(defun d6 ()
  (interactive)
  (+ (random 6) 1))

(defun 4d6drop1 ()
  (interactive)
  (let* ((rolls (list (d6) (d6) (d6) (d6)))
	 ;(_ (print `(rolls ,rolls)))
	 (result (cdr (sort rolls #'<)))
	 ;(_ (print `(result ,result)))
	 )
    (apply #'+ result)))

(defun dnd (n)
  (interactive "p")
  (let* ((buf (get-buffer-create "stats"))
	 (standard-output buf))
    (switch-to-buffer buf)
    (delete-region (point-min) (point-max))
    (loop repeat n
	  do
	  (progn 
	    (mapc #'(lambda (n) (princ (format "%2d " n)))
		  (loop repeat 6 collect (4d6drop1)))
	    (terpri)))))
    
    

(defun tryit (p)
  (interactive "p")
  (message "prefix: %S" p))
