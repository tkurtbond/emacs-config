;; This emacs code has saved me hours:
;; example: foobar -> 'foobar': foobar
(defun cpb-dict-value-at-point ()
  "Convert symbol to a dict element."
  (interactive)
  (let ((loc (bounds-of-thing-at-point 'symbol)))
    (if (null loc)
        (message "Not found")

      (let ((val (buffer-substring (car loc) (cdr loc))))
        (goto-char (car loc))
        (delete-region (car loc) (cdr loc))
        (insert (format "'%s': %s" val val))))))
