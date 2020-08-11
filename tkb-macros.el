;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-macros.el -- Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-macros.el 1.2 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'cl-lib))

(define-modify-macro notf (&rest args) not)
(define-modify-macro cdrf (&rest args) cdr)

(defmacro λ (&rest body) `#'(lambda ,@body))


;; http://groups.google.com/group/comp.lang.scheme/msg/69dccaf7b44d047e
;; http://srfi.schemers.org/srfi-2/srfi-2.html
;; I pulled this from my youtoo repo.
(defmacro and-let* (spec &rest body)
  "Evaluate a bound variable, or an expression and optionally bind it to a
variable, and if the result is true, continue to the next clause.  If all 
clauses return true, evaluate BODY.
    AND-LET* (SPEC) BODY
where 
    SPEC    = '() | (cons CLAUSE SPEC)
    CLAUSE  = (VARIABLE EXPRESSION) | (EXPRESSION) | BOUND-VARIABLE"
  (cl-labels ((expand (spec body)
		      (cond
		       ((null spec)
		        `(progn ,@body))
		       ((consp spec)
		        (let ((exp (car spec)))
		          (cond
			   ((consp exp)
			    (cond
			     ((null (cdr exp))
			      `(and ,(car exp) ,(expand (cdr spec) body)))
			     (t
			      (let ((var (car exp))
				    (val (cadr exp)))
			        `(let (,exp)
				   (and ,var ,(expand (cdr spec) body)))))))
			   (t
			    `(and ,exp ,(expand (cdr spec) body))))))
		       (t
		        (error "not a proper list" spec)))))
    (expand spec body)))

(put 'when-directory 'lisp-indent-function 1)
(defmacro when-directory (spec &rest body)
  "When a directory in SPEC exists, execute BODY.
SPEC = (DIR-FORM)
     | (VARNAME DIR-FORM)
where DIR-NAME is form that is evaluated to produce either a string
or a list or vector of strings that specify directories, and 
VARNAME is a symbol to which is bound the directory name found."
  ;;(declare (debug ([&or (form) (place form)] body)))
  (unless (and (listp spec)
	       (or (and (= (length spec) 2)
			(symbolp (car spec)))
		   (= (length spec) 1)))
    (error "misformed SPEC in when-directory: %S" spec))
  (let* ((dirvarname (gensym "when-file-dirvarname-"))
	 (varname (if (= (length spec) 1)
		      (gensym "when-file-var-")
		    (car spec)))
	 (dir-form (if (= (length spec) 1)
		       (car spec)
		     (cadr spec))))
    `(let* ((,dirvarname ,dir-form)
	    (,dirvarname (cond ((stringp ,dirvarname)
				(vector ,dirvarname))
			       ((or (listp ,dirvarname)
				    (vectorp ,dirvarname))
				,dirvarname)
			       (t
				(error "when-directory: not a string, vector, or list: %S" ,dirvarname))))
	    (,varname
	     (some #'(lambda (filename)
		       (when (file-readable-p filename) filename))
		   ,dirvarname)))
       (when ,varname
	 ,@body))))
;;(def-edebug-spec when-directory ([&or (symbolp &rest form) (&rest form)] &rest form))
  
(test (when-directory ("~/tkb") 'yes) yes)
(test (when-directory ("~/tkb/bogus") (list 'yes d)) nil)
(test (when-directory (d "~/tkb" nil) t) t error)
(test (when-directory (d "~/tkb") d) "~/tkb")
(test (when-directory (d '("~/tkb/bogus" "~/tkb")) d) "~/tkb")
(test (when-directory (d (vector "~/tkb/bogus" "~/tkb")) d) "~/tkb")
(test (when-directory (d ["~/tkb/bogus" "~/tkb"]) d) "~/tkb")
(test (when-directory (d) nil) nil error)
(test (when-directory ["~/tkb/bogus" "~/tkb"] t) nil error)
(test (when-directory (d unbound-variable) 'yes) nil void-variable)




(put 'when-file 'lisp-indent-function 1)
(defmacro when-file (spec &rest body)
  "When a filename in SPEC exists, execute BODY.
SPEC = filename
     | [filename ...]
     | (varname filename)
     | (varname [filename ...]) 
     | (varname (filename ...))
where FILENAME is a string that specifies a file and
VARNAME is a symbol to which is bound the first filename found."
  (declare (debug ([&or symbolp (symbolp form) form] body)))
  (unless (or (stringp spec)
	      (vectorp spec)
	      (and (listp spec) (= (length spec) 2)))
    (error "misformed SPEC in when-file: %S" spec))
  (let* ((varname (if (listp spec) (car spec) (gensym "when-file-var-")))
	 (filevalues (if (listp spec) (cadr spec) spec))
	 (filevalues
	  (if (stringp filevalues)
	      (vector filevalues)
	    filevalues)))
    `(let* ((,varname
	     (some #'(lambda (filename)
		       (when (file-readable-p filename) filename))
		   ,filevalues)))
       (when ,varname
	 ,@body))))

;; look at locate-library???
(defun find-load-dir (subdirs)
  "Find the first directory that exists of those in SUBDIRS."
  (let* ((subdirs (if (vectorp subdirs) subdirs (vector subdirs)))
	 (len (length subdirs))
	 (i 0))
    (catch 'found
      (while (< i len)
	(let ((subdir (aref subdirs i)))
	  (cond ((file-name-absolute-p subdir)
		 ;; not a subdir
		 (when (file-directory-p subdir)
		   (throw 'found subdir)))
		(t
		 ;; subdir
		 (let ((lp load-path))


		   (while lp
		     (let* ((basedir (car lp))
			    (dir (concat (file-name-as-directory basedir)
					 subdir)))
		       (cond ((file-directory-p dir)
			      (throw 'found dir))
			     (t
			      (setq lp (cdr lp)))))))))
	  (setq i (1+ i)))
	nil))))

(defun file-loadable-p (filename)
  (let ((lp load-path)
	(result nil))
    (while lp
      (let ((fullname (concat (car lp) "/" filename)))
	;;(message "%s" fullname)
	(cond ((or (file-readable-p fullname)
		   (file-readable-p (concat fullname ".el"))
		   (file-readable-p (concat fullname ".elc")))
	       (setq lp nil)
	       (setq result fullname))
	      (t
	       (setq lp (cdr lp))))))
    result))

(defun find-load-file (filenames)
  (let* ((filenames (if (vectorp filenames) filenames (vector filenames))))
    (catch 'found
      (map 'vector (lambda (filename)
		     (let ((loadable-p (file-loadable-p filename)))
		       (if loadable-p
			   (throw 'found loadable-p)
			 nil)))
	   filenames)
      nil)))



(put 'when-load-dir 'lisp-indent-function 1)
(defmacro when-load-dir (spec &rest body)
  "Find the subdirectories listed in SPEC on load-path and add them to 
load-path, then execute the forms in BODY, where SPEC is either DIRNAME, a 
string specifying a directory or (VAR DIRNAME) where VAR is a symbol that is 
bound to the directory if it is found."
  (declare (debug ((&rest &or (symbolp form) sexp) body)))
  (let ((varname (if (listp spec) (car spec) (gensym "WLD-directory-")))
	(subdir (if (listp spec) (cadr spec) spec)))
    `(let ((,varname (find-load-dir ,subdir)))
       (when ,varname
	 (if (member ,varname load-path)
	     (message "Path %s already in load-path" ,varname)
	   (add-to-list 'load-path ,varname)
	   (message "Adding %s to load-path" ,varname))
	 ,@body))))

(defmacro cond-load-dir (&rest body)
  "(cond-load-dir (LOADDIRS forms ...) ...)"
  `(or ,@(mapcar (lambda (wld) `(when-load-dir ,(car wld) ,@(cdr wld))) body)))

;; look at locate-library
(defmacro when-load-file (spec &rest body)
  "If the file listed in SPEC is on the load-path, then optionally load it (if
the first form of BODY is :load) and execute the forms in BODY."
  (let ((varname (if (listp spec) (car spec) (make-symbol "when-load-file-var")))
	(filename (if (listp spec) (cadr spec) spec)))
    `(let ((,varname ;;(find-load-file ,filename)
	    (locate-library ,filename)))
       (cond
	(,varname
	 (message "Found %s in load-path" ,filename)
	 ,@(cond ((eq (car body) ':load)
		 `((load-library ,varname) ,@(cdr body)))
		(t
		 `,body)))
	(t
	 (message "Did NOT find %s in load-path" ,filename))))))
(put 'when-load-file 'lisp-indent-function 1)
  

(defun tkb-is-executable-in-dir (file dir)
  (let ((pathname (expand-file-name file dir)))
    (if (file-executable-p pathname)
	pathname)))

(defun tkb-find-executable (exe path)
  (if (and (file-name-absolute-p exe)
	   (file-executable-p exe))
      exe
    (loop for dir in path
	  thereis (tkb-is-executable-in-dir exe dir))))


(put 'when-exec-found 'lisp-indent-function 1)
(defmacro when-exec-found (spec &rest body)
  "Evaluate BODY when the executable name in SPEC is found on a path, with VAR
bound to the found executable name if VAR was specified.

SPEC = exename
     | (var exename)
     | (var \"exename\")
     | (var '(\"exename\" ...))
     | (var exenames path-list)

Examples: 
    (when-exec-found \"exename\" body)
    (when-exec-found (var \"exename\") body)
    (when-exec-found (var '(\"exename\" ...)) body)
    (when-exec-found (var exenames path-list) body)"
  ;; See: (info "(elisp)Specification List")
  (declare (debug (&or [(symbolp &rest form) body]
		       [(symbolp form) body]
		       [form body]
		       )))
  ;; See (info "(elisp)Declare Form")
  (declare (indent 1))	     ; doesn't seem to work, so the put above.
  (if (not (or (stringp spec)
	       (and (listp spec)
		    (>= (length spec) 2)
		    (<= (length spec) 3))))
      (error "Invalid when-exec-found spec expression: %S" spec)
    (let ((var   (if (stringp spec) (gensym) (car spec)))
	  (exes  (if (stringp spec) spec     (cadr spec)))
	  (paths (if (stringp spec) '()      (caddr spec)))
	  (e (gensym "g-exename"))
	  (p (gensym "g-path-list")))
      `(let ((,e ,exes)
	     (,p ,paths))
	 (if (not (or (listp ,e) (vectorp ,e)))
	     (setq ,e (vector ,e)))
	 (if (null ,p)
	     (setq ,p exec-path))
	 (let ((,var (some #'(lambda (exe) (tkb-find-executable exe ,p)) ,e)))
	   (cond ((not ,var)
		  (message "None of %S available" ,e)
		  nil)
		 (t 
		  ,@body)))))))

(defvar tkb-keys-warn-only t 
"Just warn when redefining keys.")

(defmacro tkb-keys (&rest defns)
  "Ok, this is way too complicated.
Arrange for the key definitions in DEFNS to happen after tkb-keys-menus.el
is loaded, and ask the user if they should be redefined if already bound.
DEFNS is a list of (KEY-SEQUENCE BINDING) lists, optionally starting with the 
keywords `:just-warn' or `:keymap'.  If `:just-warn' is specified the warning
is printed but the user is not prompted.  If `:keymap' is specified, it must
be followed by the name of a keymap."
  (let (just-warn-p keymap)
    (while (memq (car defns) '(:just-warn :keymap))
      (let ((keyword (pop defns)))
	(case keyword
	  (:just-warn
	   (notf just-warn-p))
	  (:keymap
	   (setq keymap (pop defns))))))
    (unless keymap (setq keymap 'global-map))
    (loop for (key fun) in defns
	  collect `(define-key ,keymap ,key ,fun) into set-keys
	  collect (eval key) into keys
	  collect (if (and (consp fun) (memq (car fun) '(quote function)))
		      (cadr fun)
		    fun) into funs
	  finally return 
	  `(eval-after-load "tkb-keys-menus.el"
	     '(progn
		(multiple-value-bind (existing-bindings msg)
		    (tkb-check-bindings ',keys ',funs ,(if (list keymap)
							    keymap nil))
		  (when (or (not existing-bindings)
			    ,(if (or just-warn-p tkb-keys-warn-only)
				 `(message "Rebinding some keys:\n %s" msg)
			       `(y-or-n-p/timeout (concat "\
Some key bindings \
are already bound:\n" msg "Rebind them? "))))
		    ,@set-keys)))))))
(put 'tkb-keys 'lisp-indent-function 0)

;; This is probably not a good way to test this.
(test (macroexpand
       '(tkb-keys
	 ([f1] #'bogus)
	 ([f2] #'boguser)))
      (eval-after-load "tkb-keys-menus.el"
	'(progn
	   (multiple-value-bind
	       (bindings msg)
	       (tkb-check-bindings
		'([f1]
		  [f2])
		'(#'bogus
		  #'boguser))
	     (when
		 (or
		  (not bindings)
		  (y-or-n-p
		   (concat "Some key bindings are already bound:\n" msg "Rebind them? ")))
	       (global-set-key
		   [f1]
		 #'bogus)
	       (global-set-key
		   [f2]
		 #'boguser))))))

(defmacro with-work-buffer (buffer &rest body)
  (declare (debug (&rest form)))
  `(let ((buf (get-buffer-create ,buffer)))
     (with-current-buffer buf
       (pop-to-buffer buf)
       (erase-buffer)
       ,@body)))
(put 'with-work-buffer 'lisp-indent-function 1)      


;; Straight from *On Lisp*, p. 191.
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it, test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &rest body)
  `(aif ,test-form
	(progn ,@body)))

(defmacro awhile (expr &rest body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
	  (sym (gensym)))
      `(let ((,sym ,(car cl1)))
	 (if ,sym
	     (let ((it ,sym)) ,@(cdr cl1))
	   (acond ,@(cdr clauses)))))))

;;; tkb-macros.el
