;; http://john.freml.in/elisp-run-last-command
(defun vii-last-mx-suggestion (suggestion)
(interactive (list 
	      (read-from-minibuffer "Command to execute: "
				    (with-current-buffer "*Messages*"
				      (goto-char (point-max))
				      (search-backward "M-x")
				      (search-forward " ")
				      (thing-at-point 'symbol)))))
(call-interactively (intern suggestion)))
(define-key global-map `[(f6)] 'vii-last-mx-suggestion)
