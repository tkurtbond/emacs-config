(defun check-prefix (p)
  (interactive "p")			; was P
  (message "p: %S: type-of: %S" p (type-of p)))
