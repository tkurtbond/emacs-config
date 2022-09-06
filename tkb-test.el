(defvar tkb-testing-p nil
  "True if the 'test' macro does testing, nil if not.")

(defun toggle-test (&optional p)
  "Toggle unit testing on or off.
With prefix P, turn unit testing off if P is negative or on otherwise."
  (interactive "P")
  (setq tkb-testing-p
	(cond ((not (fboundp 'test))
	       ;; If the macro hasn't defined, we're not testing
	       nil)
	      ((not p)
	       ;; if p is nil, toggle.
	       (not tkb-testing-p))
	      ((and (numberp p) (< p 0))
	       ;; if p is -1 turn off
	       nil)
	      (t 
	       ;; if p is t and not -1, turn on.
	       t)))
  (if tkb-testing-p
      (setq test-line (concat (make-string 79 ?-) "\n\n"))
(setq test-id 0)
(defmacro test (form expected &optional expected-error)
  "Evaluate FORM and report ok if the result is equal to EXPECTED or
if EXPECTED-ERROR is signaled; otherwise report failure."
  (declare (debug (&rest form form &optional symbolp)))
  `(let ((form ',form)
	 (expected ',expected)
	 (buf (get-buffer-create "*Test-Messages*")))
     (cl-incf test-id)
     (prog1
	 (condition-case err
	     (let ((result ,form))
	       (with-current-buffer buf (goto-char (point-max)))
	       (cond
		((equal result expected)
		 (princ (format "===%d> ok: \n%s\n" test-id
				(pp-to-string form)) buf)
		 (princ (format "===%d& returned: \n%s\n" test-id
				(pp-to-string expected)) buf)
		 t)
		(t
		 (princ (format "===%d> fail: \n%s\n" test-id
				(pp-to-string form)) buf)
		 (princ (format "===%d& returned: \n%s\n" test-id
				(pp-to-string result)) buf)
		 (princ (format "===%d! expected:\n%s\n" test-id
				(pp-to-string expected)) buf)
		 nil)))
	   ,@(when expected-error
	       `((,expected-error
		  (princ (format "===%d> ok: \n%s\n" test-id
				 (pp-to-string form)) buf)
		  (princ (format "===%d& signaled expected error: %S\n"
				 test-id err) buf)
		  t)))
	   (error
	    (princ (format "===%d> fail: \n%s\n"
			   (pp-to-string form)) buf)
	    (princ (format "===%d& signaled UNEXPECTED error: %S\n" err) buf)
	    nil))
       (princ test-line buf)
       (with-current-buffer buf (goto-char (point-max))))))

    (defmacro test (form expected &optional expected-error)
      "Unit testing is turned off; try toggle-test."
      nil))
  (message "%s" (if tkb-testing-p "testing enabled" "testing disabled")))
 
(toggle-test)				;set it up the first time.
