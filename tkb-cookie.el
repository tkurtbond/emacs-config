(defvar tkb-cookie-files nil "A list of cookie files, in the format of 
yow.lines or fortune(6)")


(setq tkb-cookie-files
      (directory-files "~/lib/data/fortunes" 'full "^[-a-z_]+$"))
(defun tkb-cookie-init ()
  (interactive)
  (loop for f in tkb-cookie-files
	do (progn 
	     (message "Working on %s" f)
	     (cookie-snarf f
			   (format "Snaring %s..." f)
			   (format "Snarfed %s!" f)))))

(defun tkb-snarf ()
  (interactive)
  (cookie-snarf "~/lib/data/fortunes/fortunes" "cookie?" "cookie!"))

(defun tkb-cookie ()
  (interactive)
  (cookie "~/lib/data/fortunes/fortunes" "cookie?" "cookie!"))



(defun tkb-cookie-as-string ()
  (interactive)
  (substitute ?\  ?\n (tkb-cookie)))
