(load-file "~tkb/lib/emacs/tkb/tkb-test.el") ; before anything with (test ...)
(load-file "~tkb/lib/emacs/tkb/tkb-macros.el") ; Must come first.

;;; GNU Emacs or XEmacs?
(setq tkb-xemacs-p (string-match "xemacs" emacs-version))

(when-load-file "~tkb/lib/emacs/tkb/tkb-local.el" :load)

(when-load-dir "~tkb/local/emacs/")

(when-load-dir "/usr/lib/emacs/tkb/site-lisp")
(when-load-dir "/usr/local/share/emacs/site-lisp")
(when-load-dir "/software/public/share/emacs/site-lisp")
(when-load-dir "/sw/pub/share/emacs/site-lisp")

;;; Make sure we have a reasonable Info default directory
;;(push "/public/info/" Info-directory-list)
(when (file-directory-p "/software/public/info/"
    (push  "/software/public/info/" Info-default-directory-list)

;;; Make sure we have a reasonable info path.
(when (file-directory-p "/sw/test/GtkAda/info/")
  (push "/sw/test/GtkAda/info/" Info-default-directory-list))

(when (file-directory-p "/sw/pub/info/")
  (push "/sw/pub/info/" Info-default-directory-list))


(setq tkb-emacs-dir (expand-file-name "~tkb/lib/emacs"))
(push tkb-emacs-dir load-path)
(push (expand-file-name "~tkb/lib/emacs/tkb/others") load-path)

(when-load-dir "modula3"
  (autoload 'modula-3-mode "modula3")
  (setq auto-mode-alist
	(append '(("\\.ig$" . modula-3-mode)
		  ("\\.mg$" . modula-3-mode)
		  ("\\.i3$" . modula-3-mode)
		  ("\\.m3$" . modula-3-mode))
		auto-mode-alist)))



;;; GNU Emacs or XEmacs?
(setq tkb-xemacs-p (string-match "xemacs" emacs-version))

;;; Options and hooks
(setq scroll-step 1)
(setq auto-enable-arrow-keys nil)
(setq require-final-newline t)
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
(load-library "tkb-obsolete.el")
(load-library "tkb-text.el")
(load-library "tkb-vc.el")
(load-library "tkb-www.el")


;;; Servers
(display-time)
(cond (tkb-xemacs-p
       (gnuserv-start))
      (t
       (server-start)))


;;;; End of .emacs
