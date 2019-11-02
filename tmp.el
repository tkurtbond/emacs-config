(defun x ()
  (interactive)
  (when-exec-found (f ["/sw/versions/m64/sbcl/git/bin/sbcl"])
    (message "Found an SBCL:  %s" f)
    (setq inferior-lisp-program f)  ;I guess this makes it the default
    (let ((dir (file-name-directory f)))
      (when dir
	(setq dir (file-name-directory dir))
	(let ((sbcl-info (expand-file-name "share/info/" dir)))
	  (push sbcl-info Info-default-directory-list))))
    (list 'sbcl (list f))))


(f ["c:/Program Files/Steel Bank Common Lisp/1.0.29/sbcl.exe"
    "/sw/versions/m64/sbcl/git/bin/sbcl"
    "/sw/versions/m64/sbcl/cvs/bin/sbcl"
    "sbcl"])
