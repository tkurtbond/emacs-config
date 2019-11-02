(defun* wd ((varname dirnames) &rest body)
  (let* ((dirnames
	  (if (stringp dirnames)
	      (vector dirnames)
	      dirnames)))
    `(let ((,varname
	    (some #'(lambda (filename)
		      (when (file-directory-p filename) filename))
		  dirnames)))
       (when ,varname
	 ,@body))))
	    `
