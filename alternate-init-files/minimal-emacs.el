(setq require-final-newline 'ask)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq line-move-visual nil)
(global-set-key "\C-cC" #'compile)

;;; This is for consp.org. 
;;(load (expand-file-name "~/.roswell/helper.el"))
;;(setq inferior-lisp-program "ros -Q run")
