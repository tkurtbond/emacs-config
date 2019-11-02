;; Grumble.

(let ((old-path (split-string (getenv "PATH") ":")))
  (mapc (λ (e) (add-to-list 'old-path e))
	`(;;"/sw/versions/docutils/snapshot/2009-10-09/bin"
	  "/opt/local/bin"
	  "/opt/local/sbin"
	  ;; FIXME: do we need the following any more, since python is in
	  ;; /opt/local/bin?
	  ;;"/opt/local/Library/Frameworks/Python.framework/Versions/2.6/bin"
	  ,@(mapcar #'expand-file-name
		    '("~/local/bin"
		      "~/local/unix/bin"
		      "~/local/unix/rndbin"))
	  "/sw/versions/mew/6.7/bin"
	  ;; "/sw/versions/m64/sbcl/cvs/bin"
	  ;; "/sw/versions/32bits/clisp/cvs/bin"
	  "/sw/versions/nmh/1.3/bin"
	  ;; "/sw/versions/m64/go/rc/bin"
	  ;;"/usr/texbin"
	  ))
  ;; Plan 9
  (add-to-list 'old-path "/usr/local/plan9/bin" 'append)
  (setenv "PLAN9PORT" "/usr/local/plan9")
  
  (setenv "PATH" (mapconcat #'identity old-path ":")))
 


(mapc (λ (e) (add-to-list 'exec-path e))
      (reverse (split-string (getenv "PATH") ":")))

(add-to-list 'load-path "/sw/versions/mew/6.7/share/emacs/site-lisp")
(mapc (λ (info-dir)
	 (add-to-list 'Info-default-directory-list info-dir))
      '( "/sw/versions/mew/6.7/share/info/" "/opt/local/share/info/"))

;;(setenv "SBCL_HOME" "/sw/versions/m64/sbcl/1.0.51/lib/sbcl")
(setenv "PLAN9" "/usr/local/plan9")
