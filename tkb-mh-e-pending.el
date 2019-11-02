
(defun mh-file-command-p (file)
  "Return t if file FILE is the name of a executable regular file."
  (if (not (memq system-type '(ms-dos windows-nt cygwin)))
      (and (file-regular-p file) (file-executable-p file))
    (let ((alt-file (concat file ".exe")))
      (and (or (file-regular-p file)
	       (file-regular-p alt-file))
	   (or (file-executable-p file)
	       (file-executable-p alt-file))))))
