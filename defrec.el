;;;; defrec.el -- provides a simple record facility for Emacs-lisp

(defmacro define-record (name fields &optional rigorous)
  "`define-record' is a macro for Emacs-Lisp that provides mutable
records.  They are implemented as vectors, and are very
simple-minded.

NAME is the name of the record type.  FIELDS is an (unquoted) list of
the fields.  Functions with names make-NAME, NAME-p, NAME-<field>, and
NAME-<field>! are defined.

If RIGOROUS is true, an attempt is made to make a record
type disjoint from all other record types by making the first element
in each a unique object.  In this case, records with the same number
of fields and ordinary vectors will not satisfy the record type predicate.

Note: `==>' is read as `yields' and indicates the result of
evaluating the preceding expression.

Examples:

	(define-record myrec (a b c))     ;;; no third param means not rigorous
	(setq myrec (make-myrec 1 2 3))   ;;; <== make-myrec, not myrec-maker
	(myrec-p myrec)
	==> t
	(myrec-p [one two three])
	==> t                             ;;; because it is not rigorous
	(myrec-a myrec) 		      
	==> 1			      
	(myrec-a! myrec 'one)	      
	(myrec-a myrec)		      
	==> one			      

	(define-record myrec (a b c) t)   ;;; third param t means rigorous
	(setq myrec (make-myrec 1 2 3))
	(myrec-p myrec)
	==> t
	(myrec-p [one two three])
	==> nil                           ;;; because it is rigorous

arguments: (name fields &optional rigorous)
"
  (let ((marker (list name)))
    (` (progn
	 (, (define-record-make-predicate name fields marker))
	 (, (define-record-make-maker name fields marker)
	    (,@ field-strings))
	 (,@ (define-record-make-accessors name fields))
	 (,@ (define-record-make-setters name fields))))))

(defun define-record-make-maker (name fields marker)
  (let ((maker-name (intern (concat "make-" (symbol-name name)))))
    (` (defun (, maker-name) ((,@ fields))
	 (, (if rigorous
		(` (vector '(, marker) (,@ fields)))
	      (` (vector (,@ fields)))))))))

(defun define-record-make-predicate (name fields marker)
  (let ((predicate-name (intern (concat (symbol-name name) "-p"))))
    (` (defun (, predicate-name) (record)
	 (, (if rigorous
		(` (and (vectorp record)
			(= (length record) (1+ (, (length fields))))
			(eq '(, marker) (aref record 0))))
	      (` (and (vectorp record)
		      (= (length record) (, (length fields)))))))))))

(defun define-record-make-accessors (name fields)
  (define-record-make-accessors-helper name fields (if rigorous 1 0)))

(defun define-record-make-accessors-helper (name fields index)
  (if (null fields)
      '()
    (let ((accessor (intern (concat (symbol-name name)
				    "-"
				    (symbol-name (car fields)))))) 
      (cons (` (defun (, accessor) (record)
		 (aref record (, index))))
	    (define-record-make-accessors-helper
	      name
	      (cdr fields)
	      (1+ index))))))

(defun define-record-make-setters (name fields)
  (define-record-make-setters-helper name fields (if rigorous 1 0)))

(defun define-record-make-setters-helper (name fields index)
  (if (null fields)
      '()
    (let ((accessor (intern (concat (symbol-name name)
				    "-"
				    (symbol-name (car fields))
				    "!")))) 
      (cons (` (defun (, accessor) (record value)
		 (aset record (, index) value)))
	    (define-record-make-setters-helper
	      name
	      (cdr fields)
	      (1+ index))))))

(provide 'defrec)

;;; defrec.el ends here
