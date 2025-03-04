;;; -*- lexical-binding: t -*-
(defun tkb-count-org-subtree-entries (prefix)
  (interactive "P")
  (let ((count 0))
    (org-map-entries (lambda () (cl-incf count)) nil 'tree)
    (if prefix
        (insert (format "%d" (- count 1)))
      (message "Count: %d" (- count 1)))))
(tkb-keys ((kbd "C-c k o C") 'tkb-count-org-subtree-entries))
