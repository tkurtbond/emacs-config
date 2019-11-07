;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macos-init.el -- Set up macOS specific things.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grumble.

(when nil
  ;; I used to do this to set the PATH that programs run under emacs got.
  ;; I'm keeping it as an example of how to do this, not because it is used
  ;; currently.
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
  
    (setenv "PATH" (mapconcat #'identity old-path ":"))))
 

;; Do we still need to do this?
(when nil 
  (mapc (λ (e) (add-to-list 'exec-path e))
	(reverse (split-string (getenv "PATH") ":"))))

(if (executable-find "gls")
    (progn
      (setq insert-directory-program "gls")
      (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
  (setq dired-listing-switches "-ahlF"))


;;; end of macos-init.el


