    ;; sorta works, but doesn't seem needed
    (setq nroff-font-lock-keywords
	  (` (
	      ("\\\\\"\\(.*\\)" 1 '(, font-lock-comment-face))
	      ("^\\.[ \t]*\\([-?$a-zA-Z0-9\\\\\"_@*]+\\)" . 1)
	      ("^\\.[ \t]*[-?$a-zA-Z0-9\\\\\"_@*]+.*[ \t]\\(\\.[ \t]*[-a-zA-Z0-9\\\\\"_@*]+\\)" . 1)
	      ("\\(\\\\[nfs*][-+]*\\[[-?$.a-zA-Z0-9*@]+\\]\\)" . 1)
	      ("\\(\\\\[nfs*][-+]*([-?$.a-zA-Z0-9*@][-?$.a-zA-Z0-9*@]\\)" . 1)
	      ("\\(\\\\[nfs*][-+]*[-?$.a-zA-Z0-9*@]\\)" . 1)
	      ("^\\.[ \t]*\\(de\\|nr\\|ds\\)[ \t]+\\([-?$.a-zA-Z0-9*@()]+\\)" 2
	       '(, font-lock-function-name-face))
	      )))
