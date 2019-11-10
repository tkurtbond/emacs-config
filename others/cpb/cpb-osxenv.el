;;;  Automatically synchronize Cocoa Emacs environment variables with
;;;  the login shell environment under OS X.


(defvar osxenv-enable-at-startup t
  "If t then the environment will sync after the init file is
  finished running.  Set to nil if you want to call osxenv-setup
  manually.")


(defvar osxenv-exclude-variables '("_" "SHLVL" "TERM")
  "A list of variables that will not be copied into the
  environment of the sub processes.")


(defvar osxenv-show-env-command (concat (getenv "SHELL") " -l -c env"))


(defun osxenv-setup ()
  "TODO"
  (interactive)
  (let ((env (osxenv-get-login-env)))
    (osxenv-set-env env)
    (osxenv-set-exec-path env)))
    

(defun osxenv-set-exec-path (env)
  (let ((path (assoc "PATH" env)))
    (when path
      (dolist (i (reverse (split-string (cdr path) ":")))
        (if (not (member i exec-path))
            (setq exec-path (cons i exec-path)))))))


(defun osxenv-set-env (env)
  (dolist (i env)
    (let ((name (car i))
          (value (cdr i)))
      (cond
       ((not (member name osxenv-exclude-variables))
        (setenv name value))))))


(defun osxenv-get-login-env ()
  (with-temp-buffer
    (let ((rtn nil)
          (last nil)
          (key nil))
      (insert (shell-command-to-string osxenv-show-env-command))
      (goto-char (point-min))
      (when (re-search-forward "^\\([_a-z0-9]+\\)=" nil t)
        (setq key (match-string 1))
        (setq last (match-end 0))
        (while (re-search-forward "^\\([_a-z0-9]+\\)=" nil t)
          (setq rtn (cons (cons key (buffer-substring last (1- (match-beginning 0)))) rtn))
          (setq key (match-string 1))
          (setq last (match-end 0)))
        (setq rtn (cons (cons key (buffer-substring (match-end 0) (point-max))) rtn)))
      rtn)))


(defun osxenv-after-init-hook ()
  (when osxenv-enable-at-startup
    (osxenv-setup)))


;; Only run the hook when we have a emacs running with a cocoa powered display.
(when (eq window-system 'ns)
  (add-hook 'after-init-hook 'osxenv-after-init-hook))


(provide 'osxenv)

