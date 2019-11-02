(progn ;; t for debugging
  (setq mew-debug nil))

(setq mew-nntp-server "news.eternal-september.org")
(setq mew-nntp-user "tkurtbond")

(setq mew-imap-header-only t)

;; mew-auto-get: t: gets messages from mailbox automatically
;; C-u mew reverses mew-auto-get.
(setq mew-use-cached-passwd t)
(setq mew-passwd-timer-unit 60) 	;60 minutes (see mew-passwd-setup)
(setq mew-visit-queue-after-sending t)
(setq mew-visit-inbox-after-setting-case nil) ;???
(setq mew-use-cursor-mark t)
(setq mew-use-highlight-mouse-line t)
(setq mew-highlight-cursor-line-face 'bold)
(setq mew-mailbox-type 'mbox)
(setq mew-mbox-command "incm")
(setq mew-mbox-command-arg "-u -d /var/mail/tkb")
;; If /path/to/mbox is a file, it means "mbox".
;; If /path/to/mbox is a directory, it means "maildir".

(setq mew-pop-delete nil)

(when t
  (setq tkb-mew-smtp-port
	(if (or (string= (system-name) "KBOND")
		(eq system-type 'darwin))
	    6005 25))
  ;; If you want to change the "case", type `C'. You can use completion by
  ;; `TAB' and circular completion by `C-cTAB'.
  (setq mew-config-alist
	`((local
	   (inbox-folder "+inbox")
	   (signature-file    "~/.signature_unwind-protect.org")
	   (smtp-port         ,tkb-mew-smtp-port))
	  (mpl
	   ;; For reference: normal imap port is 143
	   (mailbox-type      "imap")
	   (proto             "%")
	   (name "T. Kurt Bond")
	   (user "kbond")
	   (mail-domain "mpl.com")
	   (imap-server       "www.wvlink.com")
	   (imap-user         "kurt_bond@mail.mpl.com")
	   (imap-delete       nil)
	   (imap-ssl          nil)
	   (inbox-folder      "%inbox")
	   (imap-ssl-port     993)
	   (smtp-server       "localhost")
	   (smtp-port         ,tkb-mew-smtp-port)
	   (signature-file    "~/.signature_mpl"))
	  (mpl-gmail
	   (mailbox-type      "imap")
	   (proto             "%")
	   (smtp-mail-from    "kbond@mpl.com")
	   (from              "kbond@mpl.com")
	   (imap-server       "imap.gmail.com")
	   (imap-user         "kbond@mpl.com")
	   (imap-delete       nil)
	   (imap-ssl          t)
	   (inbox-folder      "%inbox")
	   (imap-trash-folder "%[Gmail]/Trash")
	   (fcc               "%[Gmail]/Sent Mail")
	   (imap-ssl-port     993)
	   (smtp-server       "smtp.gmail.com")
	   (smtp-port         456)
	   (smtp-ssl          t)
	   ;; Mew can work with tls:
	   ;; http://www.mew.org/en/info/beta/mew_9.html#TLS
	   (signature-file    "~/.signature_gmail"))
	  (default
	    ;; gmail
	    (mailbox-type      "imap")
	    (proto             "%")
	    (imap-server       "imap.gmail.com")
	    (smtp-mail-from    "tkurtbond@gmail.com")
	    (from              "tkurtbond@gmail.com")
	    (imap-user         "tkurtbond@gmail.com")
	    (user              "tkurtbond")
	    (mail-domain       "gmail.com")
	    (imap-delete       nil)
	    (imap-ssl          t)
	    (inbox-folder      "%inbox")
	    (imap-trash-folder "%[Gmail]/Trash")
	    (fcc               "%[Gmail]/Sent Mail")
	    ,@(cond
	       (nil
		'((imap-ssl-port     993)
		  (smtp-server       "localhost")
		  (smtp-port         ,tkb-mew-smtp-port)))
	       (t
		'((smtp-server       "smtp.gmail.com")
		  (smtp-port         456)
		  (smtp-ssl          t))))
	    ;; Mew can work with tls:
	    ;; http://www.mew.org/en/info/beta/mew_9.html#TLS
	    (signature-file    "~/.signature_gmail"))
	  )))

;;
(setq mew-ssl-verify-level 0)
