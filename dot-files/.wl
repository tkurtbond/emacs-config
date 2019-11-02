;; -*- emacs-lisp -*-
;;doesn't work for me: (mel-use-module 'mel-q '("quoted-printable" "Q"))

(when nil
  (setq debug-on-error t
	debug-on-signal t)
  (setq debug-on-error nil
	debug-on-signal nil))

(when nil 
  (setq wl-biff-check-folder-list '("%INBOX" "%INBOX:tkurtbond@imap.gmail.com!")
	wl-biff-check-interval 60
	wl-biff-use-idle-timer nil)
  ;; Set mail-icon to be shown universally in the modeline.
  (setq global-mode-string
	(cons
	 '(wl-modeline-biff-status
	   wl-modeline-biff-state-on
	   wl-modeline-biff-state-off)
	 global-mode-string))
  (add-hook 'wl-biff-notify-hook #'ding)
  (add-hook 'wl-biff-notify-hook
	    #'(lambda () (message "wl-biff-notify-hook ran"))))

(defun my-wl-update-current-summaries ()
  (let ((buffers (wl-collect-summary)))
    (while buffers
      (with-current-buffer (car buffers)
        (save-excursion
          (wl-summary-sync-update)))
      (setq buffers (cdr buffers)))))

(add-hook
 'wl-biff-notify-hook
 '(lambda ()
    (my-wl-update-current-summaries)
    ))



;;(push "/sw/versions/wl/2.14/site-lisp/wl" load-path)
;;(setq wl-icon-directory "/sw/versions/wl/2.14/pixmaps")
;;(when-directory (d "/sw/versions/wl/snap/pixmaps")
;;  (setq wl-icon-directory d))

;; Directory where icons are placed.
;; Default: the peculiar value to the running version of Emacs.
;; (Not required if the default value points properly)
;;(setq wl-icon-directory "~/work/wl/etc")


;; SMTP server for mail posting. Default: `nil'
;;(setq wl-smtp-posting-server "localhost")
;; NNTP server for news posting. Default: `nil'
;;(setq wl-nntp-posting-server "your.nntp.example.com")

(setq elmo-imap-use-modified-utf7 t)

(setq elmo-imap4-debug nil)
;(setq elmo-imap4-debug t)
(setq elmo-pop3-debug nil)
;(setq elmo-pop3-debug t)
(cond
 (t ;; gmail
  ;; IMAP, gmail:
  (setq elmo-imap4-default-server "imap.gmail.com"
	elmo-imap4-default-user "tkurtbond@gmail.com"
	elmo-imap4-default-authenticate-type 'login
	elmo-imap4-default-port '993
	elmo-imap4-default-stream-type 'ssl

	;; For non ascii-characters in folder-names
	elmo-imap4-use-modified-utf7 t)

  ;; SMTP
  (setq wl-smtp-connection-type 'starttls
	wl-smtp-posting-port 587
	wl-smtp-authenticate-type "plain"
	wl-smtp-posting-user "tkurtbond"
	wl-smtp-posting-server "smtp.gmail.com"
	wl-local-domain "gmail.com"
	wl-message-id-domain "smtp.gmail.com")

  (setq wl-from "T. Kurt Bond <tkurtbond@gmail.com>"

	;; All system folders (draft, trash, spam, etc) are placed in the
	;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
	wl-default-folder "%inbox"
	wl-draft-folder   "%[Gmail]/Drafts"
	wl-trash-folder   "%[Gmail]/Trash"
	;; The below is not necessary when you send mail through Gmail's SMTP server,
	;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
	;; wl-fcc            "%[Gmail]/Sent"

	;; Mark sent messages as read (sent messages get sent back to you and
	;; placed in the folder specified by wl-fcc)
	wl-fcc-force-as-read    t

	;; For auto-completing foldernames
	wl-default-spec "%"))
 (nil ;; tkb
  (setq elmo-imap4-default-port 993)
  (setq elmo-imap4-default-server "unwind-protect.org")
  (setq elmo-imap4-default-stream-type 'ssl)
  (setq elmo-imap4-default-user "tkb")
  ;;(setq elmo-imap4-default-authenticate-type 'login)
  )
 (nil ;; mpl
  (setq elmo-imap4-default-authenticate-type 'clear)
  (setq elmo-imap4-default-port 993)
  (setq elmo-imap4-default-server "mail.mpl.com")
  (setq elmo-imap4-default-stream-type 'ssl)
  (setq elmo-imap4-default-user "Kurt_Bond@mail.mpl.com")
  ;;(setq elmo-imap4-default-authenticate-type 'login)
  ))

(setq mime-edit-split-message nil)
(eval-after-load "ssl"
  '(setq ssl-program-arguments
	 (append
	  (remove-if #'(lambda (e)
			 (or (and (stringp e) (string= e "-verify"))
			     (and (consp e) (eq (car e) 'int-to-string))))
		     ssl-program-arguments) '();("-crlf")
		     )))

(eval-after-load "mime-edit"
  '(progn
     (add-to-list 'mime-file-types
		  '("\\.xls$"		;MS Excel
		    ;; According to
		    ;; http://www.iana.org/assignments/media-types/application/
		    ;; this should be "application/vnd.ms-excel"
		    "application" "msexcel" nil "base64" "attachment"
		    (("filename" . file))
		    ))
     (add-to-list 'mime-file-types
		  '("\\.pdf$"
		    "application" "pdf" nil "base64" "attachment"
		    (("filename" . file))))))

(require 'mime-w3m)
(setq wl-default-folder "+inbox")

(setq wl-folder-use-frame nil)
(setq wl-summary-use-frame nil)
(setq wl-draft-use-frame nil)
(setq wl-draft-reply-buffer-style 'keep)

(setq wl-default-spec "+")

(setq wl-auto-check-folder-name "+inbox")

;; scroll-by-SPC/BS is bad because backspace isn't available in emacs under 
;; screen under putty.  I think.
(setq wl-draft-send-confirm-type 'scroll-by-j/k)

(setq wl-thread-space-str " ")

(when nil
  ("X-TKB-Extra" . (let ((x (elmo-folder-name-internal wl-summary-buffer-elmo-folder)))
			 (message "x: %S" x)
			 x)))
(setq wl-draft-config-matchone nil)
(setq tkb-wl-draft-config-added-sig nil)
(defalias 't:fts 'format-time-string)
(defalias 't:s2n 'string-to-number)
(defun t:random-year ()
  (interactive)
  (let* ((range 5000)
	 (year (t:s2n (t:fts "%G")))
	 (year (funcall (if (= 0 (random 2)) #'+ #'-) 2000 (random 5000)))
	 (year (format (if (< year 0) "%d BCE" "%d AD") (abs year))))
    (when (called-interactively-p) (message "Year: %s" year))
    year))

(setq wl-draft-config-alist
      `(
	((string-match ".*@imap.gmail.com" wl-draft-parent-folder)
	 ;; gmail smtp uses starttls; should try it sometime.
	 ;; (Maybe SSL on 587 would work?)
	 ;; http://mail.google.com/support/bin/answer.py?answer=78799
	 ("From" . "T. Kurt Bond <tkurtbond@gmail.com>")
	 ("Return-Path: T. Kurt Bond <tkurtbond@gmail.com>")
	 (wl-envelope-from . "T. Kurt Bond <tkurtbond@gmail.com>")
	 (bottom . "\n-- \nT. Kurt Bond, tkurtbond@gmail.com\n")
	 (wl-draft-config-matchone . t)
	 (tkb-wl-draft-config-added-sig . t))
	("^\\(To\\|Cc\\):.*@\\(mpl.com\\|mail.mpl.com\\|wvhtf.org\\|mail.ci.buckhannon.wv.us\\|buckhannonwv.org\\|abaca.com\\|herbsmith.net\\)"
	 ("From" . "T. Kurt Bond <Kurt_Bond@mpl.com>")
	 ("Return-Path: Kurt_Bond@mpl.com")
	 (wl-envelope-from . "Kurt_Bond@mpl.com")
	 (bottom . "\n-- \nT. Kurt Bond, Kurt_Bond@mpl.com\n")
	 (wl-draft-config-matchone . t)
	 (tkb-wl-draft-config-added-sig . t)
	 )
	("^\\(To\\|Cc\\):.*mplwebdev"
	 ("From" . "T. Kurt Bond <Kurt_Bond@mpl.com>")
	 ("Return-Path: Kurt_Bond@mpl.com")
	 (wl-envelope-from . "Kurt_Bond@mpl.com")
	 (bottom . "\n-- \nT. Kurt Bond, Kurt_Bond@mpl.com\n")
	 (wl-draft-config-matchone . t)
	 (tkb-wl-draft-config-added-sig . t)
	 )
	("^To:.*ngc.com"
	 ("From" . "T. Kurt Bond <Kurt_Bond@mpl.com>")
	 ("Return-Path: Kurt_Bond@mpl.com")
	 (wl-envelope-from . "Kurt_Bond@mpl.com")
	 (bottom . "\n-- \nT. Kurt Bond, Kurt_Bond@mpl.com\n")
	 (wl-draft-config-matchone . t)
	 (tkb-wl-draft-config-added-sig . t)
	 )
	("^To:.*polings@verizon.net"
	 ("From" . "T. Kurt Bond <Kurt_Bond@mpl.com>")
	 ("Return-Path: Kurt_Bond@mpl.com")
	 (wl-envelope-from . "Kurt_Bond@mpl.com")
	 (bottom . "\n-- \nT. Kurt Bond, Kurt_Bond@mpl.com\n")
	 (wl-draft-config-matchone . t)
	 (tkb-wl-draft-config-added-sig . t)
	 )
	((progn (message "%S" wl-draft-parent-folder)
		(or (string-match "%INBOX" wl-draft-parent-folder)
		    (string-match "%Northrop.*" wl-draft-parent-folder)))
	 ("Return-Path: Kurt_Bond@mpl.com")
	 (wl-envelope-from . "Kurt_Bond@mpl.com")
	 ("From" . "T. Kurt Bond <Kurt_Bond@mpl.com>")
	 (bottom . "\n-- \nT. Kurt Bond, Kurt_Bond@mpl.com\n")
	 )
	("^To: cpaulbond"
	 ("From" . "T. Kurt Bond <tkurtbond@gmail.com")
	 (wl-draft-config-matchone . t)
	 (bottom . (concat "\n-- \nT. Kurt Bond, tkurtbond@gmail.com\n"
			   "Randomly destroying time & space & sense since "
			   (t:random-year) "!"))
	 (tkb-wl-draft-config-added-sig . t))))

(when nil 
  (setq smtp-fqdn "unwind-protect.org")

  (multiple-value-setq (wl-smtp-posting-server wl-smtp-posting-port)
    (cond
     ((or (string= (system-name) "KBOND")
	  (eq system-type 'darwin))
      (message "Setting up Wanderlust SMTP for KBOND")
      '("localhost" 6005))
     (t
      (message "Setting up Wanderlust SMTP for local MTA")
      '("localhost" 25)))))
		     

(setq mime-edit-split-message nil)
(setq wl-alias-file "~/Mail/aliases")


(setq wl-folder-access-subscribe-alist
      '(("+" . (nil "[._]~$" "^\\+\\." "^\\+#"))))

(setq wl-summary-width nil)

(setq wl-summary-line-format "%n %E%T%P %Y/%M/%D %W %h:%m %t%[%17(%c %f%) %]%S %s")


(setq wl-user-mail-address-regexp
      (rx
       (or
	"tkurtbond@gmail.com"
	"tkurtbond@ntelos.blackberry.com"
	(and
	 ;; Various usernames I've used
	 (or "kbond" "kurt_bond" "tkb" "tkbond" "tkurtbond")
	 "@"
	 ;; Various mail hosts.
	 (or "tkb.mpl.com"
	     (and (opt (* print) ".")
		  (or "unwind-protect.org"
		      "mpl.com"
		      "wvlink.com")))))))

(when nil ;; check against these with regexp-builder
"kurt_bond@mail.mpl.com
tkb@tkb.mpl.com
tkb@mpl.com
tkbond@mail.mpl.com
tkb@www.wvlink.com
tkb@random.mpl.com
tkb@randommpl.com"
)

(setq wl-use-petname nil)

(push
 `(((type . text)
    (subtype . plain)
    (mode . "play")
    (method . wl-mime-display-text/plain)
    (major-mode . wl-original-message-mode)
    (disposition-type . attachment))
   . 0)
 mime-acting-situation-example-list)


(define-prefix-command 'tkb-wl-summary-map)
(define-key wl-summary-mode-map "K" #'tkb-wl-summary-map)
(define-key wl-summary-mode-map "k" #'wl-summary-delete)
(define-key tkb-wl-summary-map "v"
  #'(lambda () (interactive) (wl-summary-toggle-disp-msg 'off)))
(define-key tkb-wl-summary-map "p" #'wl-summary-pack-number)
(define-key tkb-wl-summary-map "a" #'(lambda ()
				       (interactive)
				       (wl-local-address-init)))

;; from Ron Isaacson
;; Show a mark next to messages were I appear in the To or Cc list (to
;; distinguish mail sent explicitly to me from mail I get via
;; mailgroups)

(defun ri:wl-summary-line-to-me ()
  (let ((all-addresses (append
                        (elmo-message-entity-field wl-message-entity 'to t)
                        (elmo-message-entity-field wl-message-entity 'cc t)))
	(to-me nil))
    (while (and all-addresses
                (not to-me))
      (setq to-me (wl-address-user-mail-address-p (car all-addresses)))
      (setq all-addresses (cdr all-addresses)))
    (if to-me "@" " ")))

(setq wl-summary-line-format-spec-alist
      (put-alist '?E
		 '((ri:wl-summary-line-to-me))
		 wl-summary-line-format-spec-alist))


(progn 
  (defun my-mime-preview-play-current-entity-with-doc-view ()
    "Play part using DocView."
    (interactive)
    (let ((entity (get-text-property (point) 'mime-view-entity))
	  name)
      (when entity
	(if (setq name (mime-entity-safe-filename entity))
	    (setq name (file-name-nondirectory (eword-decode-string name)))
	  (setq name (make-temp-name "doc-view-")))
	(let ((pop-up-frames t))
	  (pop-to-buffer (generate-new-buffer name)))
	(set-buffer-multibyte nil)
	(insert (mime-entity-content entity))
	(set-buffer-modified-p nil)
	(setq buffer-file-name name)
	(condition-case err
	    (doc-view-mode)
	  (error (message "%s" (error-message-string err))))
	(use-local-map (copy-keymap doc-view-mode-map))
	(local-set-key
	 "q"
	 (lambda ()
	   (interactive)
	   (delete-frame (prog1
			     (selected-frame)
			   (quit-window 'kill))))))))

  (add-hook
   'mime-view-mode-hook
   (lambda ()
     (local-set-key
      "V"
      'my-mime-preview-play-current-entity-with-doc-view))))
