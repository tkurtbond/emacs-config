;; -*- emacs-lisp -*-
(global-set-key "\M-?" 'help-command)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-`" 'query-replace-regexp)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cm" 'compile)
(global-set-key "\C-ct" 'toggle-truncate-lines)

;; Fix spliting the window.
(setq split-height-threshold nil)
(setq split-width-threshold nil)
