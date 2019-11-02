(require 'cl)
(case 'nntp
;;;;;;;;;;
  (imap 
   (case 3
     (1
      (setq gnus-select-method '(nnimap "www.wvlink.com"
					(nnimap-address "www.wvlink.com")
					(nnimap-stream ssl)
					(nnimap-list-pattern ("INBOX" ""))
					;(nnimap-server-port 433)
					;(nnimap-authenticator cram-md5)
					)))
     (2
      (setq gnus-select-method '(nnimap "tkb.mpl.com"
					(nnimap-address "tkb.mpl.com")
					(nnimap-stream ssl)
					;(nnimap-port 993)
					;(nnimap-port 433)
					;(nnimap-list-pattern ("
					)))
     (3
      (setq ;;gnus-secondary-select-methods
       gnus-select-method
       '(nnimap "www.wvlink.com"
		(nnimap-address "www.wvlink.com")
		(nnimap-authenticator login)
		(nnimap-stream ssl)
		;;(remove-prefix "INBOX.")
		;;(nnimap-authinfo-file "/home/erik/.imap-authinfo")))
		)
       )))
   

   (setq imap-debug t)
   (setq gnus-message-archive-method
	 '(nnimap "imap-mail"))

   (setq nnimap-split-rule
	 '(("INBOX.junk" "^Subject:.*test")
	   ("mbox" ".*")))
   )

;;;;;;;;;;
  (nntp

   (cond (nil
	  (setq gnus-select-method '(nntp "news.gmane.org"))
	  ;; Look in .authinfo
	  (setq gnus-secondary-select-methods
		'( ;;(nntp "news.motzarella.org")
		  (nntp "news.eternal-september.org")
					;(nntp "pyramid.sjgames.com")
		  )))
	 (t
	  (setq gnus-select-method '(nntp "news.eternal-september.org"))
	  (setq gnus-secondary-select-methods
		'((nntp "news.gmane.org")))))

   (setq gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23f%]%) %&user-date; %s\n")

   (setq gnus-user-date-format-alist 
	 '(((gnus-seconds-today)
	    . "%k:%M")
	   (604800 . "%a %k:%M")
	   ((gnus-seconds-month)
	    . "%a %d")
	   ((gnus-seconds-year)
	    . "%m/%d")
	   (t . "%Y-%m-%d ")))

   ))
