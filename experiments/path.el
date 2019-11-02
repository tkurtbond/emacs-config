;; Now in tkb-experimental.el

(defun path-get (&optional env-variable)
  (unless env-variable (setq env-variable "PATH"))
  (split-string (getenv "PATH") ":"))

(defun path-set (path-elements &optional env-variable)
  (unless env-variable (setq env-variable "PATH"))
  (setenv "PATH" (mapconcat #'identity path-elements ":")))

(defun path-prepend (directories &optional env-variable)
  (let ((path (path-get env-variable)))
    (loop for dir in (reverse directories)
	  do (unless (member* dir path :test #'string-equal)
	       (push dir path)))
    (path-set path env-variable)))

(defun path-delete (directories &optional env-variable)
  (let ((path (path-get env-variable)))
    (loop for dir in directories
	  do (setq path (delete* dir path :test #'string-equal)))
    (path-set path env-variable)))

(defun path-append (directories &optional env-variable)
  (path-set (append (path-get env-variable) directories) env-variable))
