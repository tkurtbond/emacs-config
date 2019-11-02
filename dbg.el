(setq debug-on-error t)

(when nil
  ;; Note that M-: (error "hello") will enter the debugger when 
  (defun x ()
    (interactive)
    (error "blast it: %S: %S" debug-on-error eval-expression-debug-on-error)))
