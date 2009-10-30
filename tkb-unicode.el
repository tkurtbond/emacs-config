;; incomplete
(defun tkb-read-unidata ()
  (setq tkb-unicode-character-alist '())
  (save-excursion
    (let ((buf (find-file "~/tmp/UnicodeData.txt")))
      (goto-char (point-min))
      (while (not (eobp))
	(when (looking-at "^\\([0-9AaBbCcDdEeFf]+\\);\\([^;]*\\);[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;\\([^;]*\\);[^;]*;[^;]*;[^;]*;[^;]*$")
	  (let* ((codepoint* (match-string 1))
		 (codepoint (string-to-number codepoint* 16))
		 (character-name (match-string 2))
		 (description (match-string 3))
		 (tag  (if (and (not (= 0 (length character-name)))
				(/= ?< (aref character-name 0)))
			   character-name
			 description)))
	    (when nil 
	      (message "codepoint*: %s codepoint: %s character-name: %s description: %s"
		       codepoint* codepoint character-name description))

	    (unless (string-match "\\(First\\|Last\\)>$" character-name)
	      (setq tkb-unicode-character-alist (cons (cons tag codepoint)
						      tkb-unicode-character-alist)))))
	(forward-line)))))

(defun tkb-unicode-character-insert (arg &optional argname)
  "Insert a Unicode character by character name. If a prefix is given, the character will be inserted regardless of whether or not it has a displayable glyph; otherwise, a numeric character reference is inserted if the codepoint is not in the unicode-glyph-list. If argname is given, it is used for the prompt. If argname uniquely identifies a character, that character is inserted without the prompt."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (uniname (if (stringp argname) argname ""))
	 (charname
	  (if (eq (try-completion uniname tkb-unicode-character-alist) t)
	      uniname
	    (completing-read
	     "Unicode name: "
	     tkb-unicode-character-alist
	     nil t uniname)))
	 codepoint glyph)
    (setq codepoint (cdr (assoc charname tkb-unicode-character-alist)))
    (ucs-insert codepoint)))

