;;; tkb-blog.el --- things to help with my blog      -*- lexical-binding: t; -*-
;; Author: T. Kurt Bond <tkurtbond@gmail.com>

(defvar tkb-blog-edited-string
  "\n\n*Last edited: 2021-07-15 17:46:40 EDT*

\.\.
   Local Variables:
   time-stamp-format: \"%Y-%02m-%02d %02H:%02M:%02S %Z\"
   time-stamp-start: \"\\\\*Last edited:[ \\t]+\\\\\\\\?\"
   time-stamp-end: \"\\\\*\\\\\\\\?\\n\"
   time-stamp-line-limit: -20
   End:\n")

(defun tkb-add-blog-edited ()
  (interactive)
  (unless (save-excursion
            (goto-char (point-min))
            (search-forward "*Last edited:" nil t))
    (goto-char (point-max))
    (insert tkb-blog-edited-string)))
