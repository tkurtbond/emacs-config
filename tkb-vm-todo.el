;;; -*- emacs-lisp -*-
(require 'cl)

(setq months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
(defun month-number (name)
  (let ((n (position name months :test (function string-equal))))
    (if n
	(1+ n)
      n)))

(defun tkb-vm-todo-filename ()
  (interactive)
  (message "tkb-vm-todo-filename")
  (concat (match-string 5) "-"
	  (format "%02d" (month-number (match-string 4))) 
	  "/TODO"))

;Date: Thu, 30 May 2002 12:40:42 -0400 (EDT)
;"[a-za-za-z], \\(*[0-9]+\\) \\([a-z]+\\) \\([0-9]+\\) [0-9]+:[0-9]+"
(defun tkb-vm-todo ()
  (interactive)
  (let ((vm-auto-folder-alist
	 '(("Date:"
	    ("\\(\\([A-Za-z]+\\), *\\([0-9]+\\) *\\([A-Za-z]+\\) *\\([0-9]+\\).*\\)" .
	     (progn
	       (message "progn")
	       (tkb-vm-todo-filename))))
	   ))
	(vm-folder-directory "~/vm/"))
    (call-interactively 'vm-save-message)))
  
(define-key vm-summary-mode-map "T" (function tkb-vm-todo))


(when nil
  (fset 'tkb-orig-vm-save-message (symbol-function 'vm-save-message))

  (defadvice vm-save-message (before tkb-vsm-before activate)
    (message "%S\n" vm-auto-foldler-alist)))
