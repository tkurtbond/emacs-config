;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-functions.el -- these are functions that are generic emacs functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-functions.el 1.2 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun empty-string-p (s)
  "Return true if S is empty"
  (zerop (length s)))

(defvar tkb-align-column 79
  "Put the last character of this line in `tkb-align-column'")

(defun tkb-align-end (col)
  "Align the end of this line at character `tkb-align-column',
or at the column specified by the prefix arg."
  (interactive "P")
  (let ((col (if col col tkb-align-column)))
    (let ((current-end (save-excursion
			 (end-of-line)
			 (current-column))))
      (if (< current-end col)
	  (insert (make-string (- col current-end) ? ))))))

(defun tkb-get-clipboard ()
  (interactive)
  (insert (x-get-clipboard)))


(defun tkb-show-position ()
  (interactive)
  (let* ((params (frame-parameters))
       (top (assq 'top params))
       (left (assq 'left params))
       (height (assq 'height params))
       (width (assq 'width params)))
    (message "%s %s %s %s" top left height width)))


(defun tkb-insert-buffer-filename (prefix)
  (interactive "P")
  (insert (if prefix (buffer-file-name)
	    (file-name-nondirectory
	     (buffer-file-name)))))

(defun tkb-count-region ()
  (interactive)
  (message "%d characters in region."
	   (abs (- (mark) (point)))))

(defun tkb-pad-string (s)
  "Pad string S with spaces between each character"
  (interactive "sString: ")
  (let ((l (length s)) (s2 (char-to-string (aref s 0))) (i 1))
    (while (< i l)
      (setq s2 (concat s2 " " (char-to-string (aref s i))))
      (setq i (1+ i)))
    (insert s2)))

(defun tkb-ibn-padded (up)
  "Insert Buffer Name with spaces between each character. With arg, upcase."
  (interactive "P")
  (tkb-pad-string (cond (up (upcase (buffer-name)))
			(t (buffer-name)))))

(defun tkb-ibn (up)
  "Insert Buffer Name. With arg, upcase."
  (interactive "P")
  (insert (cond (up (upcase (buffer-name)))
		(t (buffer-name)))))
(tkb-keys ("\C-ckB" #'tkb-ibn))

(defun tkb-stretch-string (s)
  (interactive "sString: ")
  (let* ((len (length s))
	 (ns (make-string (* len 3) ?\s))
	 (i 0))
    (while (< i len)
      (let ((c (aref s i))
	    (n (* i 3)))
	(aset ns n c)
	(aset ns (+ n 1) c)
	(aset ns (+ n 2) c))
      (incf i))
    (insert ns)))


(defun tkb-insert-user-name ()
  (interactive)
  (insert (user-full-name)))

(defun tkb-insert-login-name ()
  (interactive)
  (insert (user-login-name)))

(defun tkb-find-next-tag ()
  (interactive)
  (find-tag nil t nil))

(defun tkb-mail-self-note (self)
  (interactive "P")
  (message "mail-self: %S" self)
  (let ((to (if self
		(if (stringp self)
		    self
		  (completing-read "Username: "
				   `(((concat "tkb+note@" mail-host-address)  1)
				     ("kurt_bond@mpl.com" 2))))
	      (concat "tkb+note@" mail-host-address))))
    ;; Whoa, way too fiddly. ???
    (let ((mail-self-blind (and self
				(or (and (numberp self)
					 (< self 0))
				    (eq self '-)))))
      (compose-mail to)
      (mail-subject))))

(defun tkb-mail-self-note-mpl ()
  (interactive)
  (tkb-mail-self-note "kurt_bond@mail.mpl.com"))

(defun tkb-mail-bosses ()
  (interactive)
  (random t)
  (let ((bosses ["Hayes_Theiling@mail.mpl.com" "Linda_Wellings@mail.mpl.com"
		 "Phil_Loftis@mail.mpl.com"])
	to)
    (while (not (= (length to) 3))
      (let ((user (aref bosses (random 3))))
	(unless (member user to)
	  (push user to))))
    (message "To: %S" to)
    (compose-mail (mapconcat (function (lambda (n) n)) to ", "))
    (mail-subject)))
    

(defun tkb-edit-incoming-links ()
  (interactive)
  (let ((tkb-links-file "Incoming/incoming.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-new-links ()
  (interactive)
  (let ((tkb-links-file "new-links.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-tkb-links ()
  (interactive)
  (let ((tkb-links-file "tkb-links.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-www-links ()
  (interactive)
  (let ((tkb-links-file "www-links.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-phones-data ()
  (interactive)
  (forms-find-file "~/tkb/Data/Phones/phones.fmt"))


(defun tkb-insert-cut-buffer ()
  (interactive)
  (insert (x-get-cut-buffer)))

(defun tkb-reset-isearch-highlight ()
  (interactive)
  (setq isearch-lazy-highlight (not isearch-lazy-highlight)))

(defun* tkb-check-bindings (keys-list &optional bindings-list (keymap global-map))
  "Check if the key sequences in KEYS-LIST are already defined;
BINDINGS-LIST optionally contains the new bindings (functions)."
  (unless bindings-list
    (setq bindings-list (make-list (length keys-list) nil)))
  (loop for keys in keys-list
	for new-binding in bindings-list
	for binding = (lookup-key keymap keys)
	;; lookup-key returns a number for key sequences that don't have
	;; valid sequence of prefix characters in the keymap.
	when (and binding (not (numberp binding)))
	collect (cons keys binding)
	into bindings
	and collect (if new-binding
			(format "  %s is %S but will become %S\n" (key-description keys) binding new-binding)
		      (format "  %s is %S\n" (key-description keys) binding))
	into msgs
	finally return (values bindings (apply #'concat msgs))))

(when nil
  (tkb-check-bindings '("\C-ckR"))
  (tkb-check-bindings '("\C-ckr"))
  (tkb-keys ("\C-ckP" #'ding))
  (tkb-check-bindings '("\C-ckp"))
  )

(defun tkb-rfc822-date-time (&optional time)
  ;; http://www.faqs.org/rfcs/rfc822.html
  (format-time-string "%a, %e %b %y %T %Z" time t))

(defun trim-end (s)
  (replace-regexp-in-string "\\( \\|\f\\|\t\\|\n\\)+$" "" s))

(defun trim-start (s)
  (replace-regexp-in-string "^\\( \\|\f\\|\t\\|\n\\)+" "" s))

;;; end of tkb-functions.el
