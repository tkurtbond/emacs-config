(setq tkb-mh-e-el-file-loaded t)
; default: (setq mh-compose-insertion 'MML)
(setq mh-identity-default "default")
(cond
 (nil 
  (setq mh-identity-list
	'(("default"
	   (("From" . "\"T. Kurt Bond\" <tkb@unwind-protect.org>")
	    (":signature" . "~/.signature_unwind-protect.org")))
	  ("mpl"
	   (("From" . "\"T. Kurt Bond\" <Kurt_Bond@mpl.com>")
	    ("Organization" . "MPL Corporation")
	    ("DCC" .
	     "\"T. Kurt Bond\" <Kurt_Bond@mpl.com>, tkb@unwind-protect.org")
	    (":signature" . "~/.signature_mpl"))))))
 (t
  (setq mh-identity-list
	'(("tkb.mpl.com" (("From" . "\"T. Kurt Bond\" <tkb@tkb.mpl.com>")
			  (":signature" . "~/.signature-old")))
	  ("gmail" (("From" . "tkurtbond@gmail.com")
		    (":signature" . "~/.signature-gmail")))
	  ("mpl" (("From" . "kurt_bond@mpl.com")
		  (":signature" . "~/.signature-mpl")))
	  ("leo" (("From" . "thomas.bond@leo.gov")
		  (":signature" . "~/.signature-leo")
		  ("Return-Path" . "<thomas.bond@leo.gov>")))))))
(setq mh-alias-expand-aliases-flag t)
(setq w3m-imitate-widget-button t)
(setq mh-display-buttons-for-alternatives-flag t)

(defmacro null-string-to (expr nullreplr)
  (let ((expr-name (gensym "null-string-to-")))
    `(let ((,expr-name ,expr))
       (if (stringp ,expr-name)
	   (if (string-equal ,expr-name "")
	       ,nullreplr
	     ,expr-name)
	 ,expr-name))))

(defun tkb-get-received-date (rcvd)
  "return a date or nil"
  (when (string-match ".*;[ \t]*\\(.*\\)$" rcvd)
    (apply #'encode-time (parse-time-string (match-string 1 rcvd)))))

(defun tkb-get-received-date-string ()
  "return a date or nil"
  (let ((rcvd (mh-get-header-field "Received:")))
    (when (string-match ".*;[ \t]*\\(.*\\)$" rcvd)
      (match-string 1 rcvd))))

(setf tkb-splitters nil)

(defun tkb-mh-e-read-folder-map ()
  (interactive)
  (save-current-buffer
    (let* ((buf (find-file-noselect "~/lib/emacs/tkb/tkb-folder-map.el"))
	   (_ (set-buffer buf))
	   (_ (goto-char (point-min)))
	   (mapping (read buf)))
    
      (setf tkb-splitters
	    (loop for (tag . folderspecs) in mapping
		  append `((,(concat tag ":") ,@folderspecs)) into mh-mapping
		  finally return mh-mapping)))))

(tkb-mh-e-read-folder-map)

(defun tkb-mh-default-folder ()
  (message "well, we got called")
  (let* ((date-string (null-string-to
		       (mh-get-header-field "Date:")
		       (tkb-get-received-date-string)))
	 (date (date-to-time date-string))
	 (date-folder (format-time-string "+y%Y/m%m/" date)) ;wanderlust ignores directories that are just numbers
	 (folder
	  (catch 'found
	    (loop for (header-name . name-value-pairs) in tkb-splitters
		  with header-value = (mh-get-header-field header-name)
		  do (progn
		       (let ((header-value (mh-get-header-field header-name)))
			 (when nil 
			   (message "header-name: %s header-value: %s"
				    header-name header-value))
			 (when (not (string-equal "" header-value))
			   (loop for (hv . ln) in name-value-pairs
				 do (when (string-match hv header-value)
				      (throw 'found ln))))))))))
    (concat date-folder folder)))

(setq mh-default-folder-for-message-function
      #'tkb-mh-default-folder)
