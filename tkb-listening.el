(defun tkb-listening-prefixes-not-regexp (prefixes)
  ;; I wish emacs regexps had (?! ).
  (concat "[^" (apply #'concat prefixes) "]"))

(defvar tkb-listening-prefixes '("▪︎"  "✓︎"  "🗙"  "≐"  "😑"  ":"))
(defvar tkb-listening-prefixes-not-regexp
  (concat "^\\* " (tkb-listening-prefixes-not-regexp tkb-listening-prefixes)))

(defun tkb-listening-what-next ()
  (interactive)
  (goto-char (point-min))
  (occur tkb-listening-prefixes-not-regexp))
