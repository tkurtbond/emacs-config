;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; .emacs -- TKB's emacs file. -*- emacs-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: .emacs 1.3 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when nil
  ;; I'm not sure how useful this is, since load prints what file its loading
  ;; anyway.
  (defadvice load (around protect)
    (let ((args (ad-get-args 0)))
      (message "Before loading: %s" args)
      ad-do-it
      (message "After loading: %s" args)))
  (ad-activate 'load))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; temporary debugging code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For debugging when my abbrevs table gets changed so that emacs isn't
;; always asking to save them.
(cond
 ;; Set to true to be debugging
 (nil
  (setq message-log-max 1000)
  (defmacro tkb-dbg (&rest body)
    `(progn ,@body)))

 ;; not debugging!
 (t
  (defmacro tkb-dbg (&rest body)
    (progn))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; end of temporary debugging code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tkb-dbg (load-file "~/lib/emacs/emacs-config/tkb-debug-abbrevs.el"))

(load-file "~/lib/emacs/emacs-config/tkb-test.el") ; before anything with (test ...)
(load-file "~/lib/emacs/emacs-config/tkb-macros.el") ; Must come first.

;;; GNU Emacs or XEmacs?
(setq tkb-xemacs-p (string-match "xemacs" emacs-version))

(message "after to ~/abbrev.def")
(when (file-readable-p "~/abbrev.def")
  (setq tkb-loaded-abbrevs t)
  (tkb-dbg (tkb-show-abbrev-settings))
  (quietly-read-abbrev-file "~/abbrev.def")
  (tkb-dbg (tkb-show-abbrev-settings)))
(message "after to ~/abbrev.def")



(when (file-exists-p "~/lib/emacs/emacs-config/tkb-local.el")
  (load-file "~/lib/emacs/emacs-config/tkb-local.el"))

(when (file-exists-p "~/local/emacs/")
  (push "~/local/emacs/" load-path))

(when-directory (d "/usr/lib/emacs/emacs-config/site-lisp") (push d load-path))
(when-directory (d "/usr/local/share/emacs/site-lisp") (push d load-path))
(when-directory (d "/software/public/share/emacs/site-lisp") (push d load-path))
(when-directory (d  "/sw/pub/share/emacs/site-lisp") (push d load-path))

;;; Make sure we have a reasonable load-path
(when (file-directory-p "/sw/test/GtkAda/info/")
  (push "/sw/test/GtkAda/info/" Info-default-directory-list))

(when (file-directory-p "/sw/pub/info/")
  (push "/sw/pub/info/" Info-default-directory-list))


(when-directory (d (expand-file-name "~/lib/emacs/emacs-config"))
  (setq tkb-emacs-dir d)
  (add-to-list 'load-path tkb-emacs-dir))

(cl-loop for d across ["~/lib/emacs/others"
                       "~/lib/emacs/others/misc"
                       ;; not right now.  "~/lib/emacs/others/old-ada-mode"
                       ]
      do (progn
	   (when-directory (o (expand-file-name d))
	     (message "adding %s to load-path" o)
	     (add-to-list 'load-path o))))

(when-load-dir "modula3"
  (autoload 'modula-3-mode "modula3")
  (setq auto-mode-alist
	(append '(("\\.ig$" . modula-3-mode)
		  ("\\.mg$" . modula-3-mode)
		  ("\\.i3$" . modula-3-mode)
		  ("\\.m3$" . modula-3-mode))
		auto-mode-alist)))


;;; Make sure we have a reasonable Info default directory
;(push "/public/info/" Info-directory-list)
(when-directory (d "/software/public/info/")
  (push d Info-default-directory-list))

;;; GNU Emacs or XEmacs?
(setq tkb-xemacs-p (string-match "xemacs" emacs-version))

;;; Options and hooks
(setq scroll-step 1)
(setq auto-enable-arrow-keys nil)
(setq require-final-newline 'ask)
(add-hook 'write-file-hooks 'time-stamp)

;; Emacs disabled features
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)


;;; Libraries
;(load-library "uncompress")

;;; Load sub-files
;; The order these are loaded in may matter.

(load-library "tkb-functions.el")
(load-library "tkb-experimental.el")
(load-library "tkb-formatting.el")
(load-library "tkb-time.el")
(load-library "tkb-duration.el")
;; 2008-07-11: ~/lib/emacs-obsolete (load-library "tkb-gnus.el")
(load-library "tkb-keys-menus.el")
(load-library "tkb-lang.el")
(load-library "tkb-links.el")
(load-library "tkb-mail.el")
(load-library "tkb-text.el")
(load-library "tkb-vc.el")
(load-library "tkb-unicode.el")
(load-library "tkb-www.el")
(load-library "tkb-listening")


;;; Servers

;; My UI does this now.
;; (display-time)
(cond (tkb-xemacs-p
       (gnuserv-start))
      (t
       (unless (getenv "EMACS_NO_SERVER")
	 (server-start))))

;;;; End of unix-init.el
