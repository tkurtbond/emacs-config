(setq fortune-dir "")
;; Cygwin fortune doesn't understand c:/, so we'll make sure
;; not to use expand-file-name.
(cond ((eq system-type 'cygwin)
       (setq fortune-file "/home/tkb/lib/data/fortunes"))
      (t
       (setq fortune-file (expand-file-name "~/lib/data/fortunes"))))

(when nil
  (ad-disable-advice 'fortune-in-buffer 'around 'tkb-fortune-in-buffer-ad))

(when nil 
  (defadvice fortune-in-buffer (around tkb-fortune-in-buffer-ad activate)
    "Advice for odd MS Windows/Cygwin clash: don't expand the fortune-file name,
since that results in <DRIVELETTER>:<PATH>, which Cygwin fortune finds 
finds confusing."
    (let ((fortune-buffer (or (get-buffer fortune-buffer-name)
			      (generate-new-buffer fortune-buffer-name)))
	  (fort-file (or file fortune-file)))
      (save-excursion
	(set-buffer fortune-buffer)
	(toggle-read-only 0)
	(erase-buffer)

	(if fortune-always-compile
	    (fortune-compile fort-file))

	(call-process
	 fortune-program    ;; programm to call
	 nil fortune-buffer t ;; INFILE BUFFER DISPLAYP
	 (concat fortune-program-options fort-file))))))

(message "**** tkb-fortune.el")
