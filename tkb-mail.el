;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-mail.el -- mail customization and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader$
;;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when nil 
  (setq mail-host-address "unwind-protect.org"))

(when nil ;; ??? FIXME: I'm doing this in custom.el and below
  ;; MH expects things under ~/Mail while feedmail.el expects things under 
  ;; ~/mail and since I sync to MSWoe and its' case insenstitivity
  ;; this is problematical.
  (setq feedmail-queue-directory
	(expand-file-name "~/Mail/feedmail/q"))
  (unless (file-directory-p feedmail-queue-directory)
    (make-directory feedmail-queue-directory t))
  (setq feedmail-queue-draft-directory
	(expand-file-name "~/Mail/feedmail/drafts"))
  (unless (file-directory-p feedmail-queue-draft-directory)
    (make-directory feedmail-queue-draft-directory t))
  )

;; Mailcrypt
(when nil
  (push "/usr/local/share/emacs/site-lisp" load-path)
  (load-library "mailcrypt")
  (mc-setversion "gpg")
  (autoload 'mc-install-write-mode "mailcrypt" nil t)
  (autoload 'mc-install-read-mode "mailcrypt" nil t)
  (add-hook 'mail-mode-hook 'mc-install-write-mode)
  (add-hook 'vm-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-mail-mode-hook 'mc-install-write-mode)
  (add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'message-mode-hook 'mc-install-write-mode)
  (add-hook 'news-reply-mode-hook 'mc-install-write-mode))

(fset 'tkb-burst-digest-and-back
   [?\C-x ?/ ?a ?* return ?\C-x ?j ?a ?\C-n])

;(fset 'undigest-and-back
;   [?* return ?\C-[ ?< ?\C-[ ?n ])

;(fset 'gmast-l-edit
;   [?\C-c ?# ?^ ?\\ ?  ?\\ ?  ?\\ ?  ?- ?- return ?- ?- return ?!])

(fset 'searoom-ins-digest
   "\C-p\C-p\C-[\C-f\C-[\C-f\C-[\C-fe digest\C-c\C-c")

;;; This one deletes anything after the end of a digest, like the headers
;;; that MX switches to the end of the message.
(fset 'to-undigest
   "\C-[>\C-r*******\C-a\C-ne\C-w\C-c\C-c")
(defun my-undigest ()
  (interactive)
  (goto-char (point-max))
  (rmail-edit-current-message)
  (if (re-search-backward "^------------" nil t)
      (delete-region (point) (point-max)))
  (rmail-cease-edit)
  (undigestify-rmail-message))


;; suppress the check that causes the sender header to be generated.
(setq message-syntax-checks '((sender . disabled)))


;;; Outgoing mail
(setq user-full-name "T. Kurt Bond")

; Default is:
;(setq send-mail-function 'sendmail-send-it)

(setq tkb-is-root (string-equal (user-real-login-name) "root"))

(setq frame-title-format
      '(multiple-frames
	("emacs %b: " (tkb-is-root (:eval (concat "root@" (system-name)))  user-mail-address))
	("emacs " (tkb-is-root (:eval (concat "root@" (system-name))) user-mail-address))))

(unless tkb-xemacs-p
  (setq icon-title-format
	'(multiple-frames ("emacs %b: " (tkb-is-root "root" user-mail-address))
			  ("emacs " (tkb-is-root "root" user-mail-address)))))

;; This is an old version I ought to get rid of sometime.
(when nil
  (defun was-tkb-set-mail (host smtp-host &optional user)
    (if (not user) (setq user "tkb"))
    (setq user-mail-address (concat user "@" host))
    ;; I think you have to set the following before loading smtpmail.
    (setq smtpmail-default-smtp-server smtp-host)
    (setq smtpmail-local-domain nil)
    (setq send-mail-function 'smtpmail-send-it)
    (setq message-send-mail-function 'smtpmail-send-it)
    (setq smtpmail-debug-info t)
    (load-library "smtpmail")))

(when nil

  (defun tkb-set-mail (host smtp-host &optional user port)
    (if (not user) (setq user "tkb"))
    (if (not port) (setq port 25))
    ;;broken: (setq user-mail-address (concat "T. Kurt Bond <" user "@" host ">"))
    (setq user-mail-address (concat user "@" host))
    ;; I used to think you had to set the following before loading smtpmail.
    ;; Now I'm not sure.
    (setq smtpmail-default-smtp-server smtp-host)
    ;;(setq mail-default-headers (concat "BCC: " user-mail-address "\n"))
    (setq mail-self-blind t)
    (setq message-default-mail-headers
	  (concat "Bcc: " user-mail-address
		  ;; always copy mail to tkb@unwind-protect.org.
		  (if (string-equal user-mail-address "tkb@unwind-protect.org")
		      ""
		    "\nBcc: tkb@unwind-protect.org")))
    (setq mail-signature (concat "\n-- \nT. Kurt Bond, " user-mail-address "\n"))
    (setq message-signature (concat "T. Kurt Bond, " user-mail-address "\n"))
    (setq smtpmail-smtp-server smtp-host)
    (setq smtpmail-local-domain nil)
    (setq smtpmail-smtp-service port)
    (cond			      ; Don't try to use sendmail (:-)
     (nil			      ; using smtpmail
      (setq send-mail-function 'smtpmail-send-it)
      (setq message-send-mail-function 'smtpmail-send-it))
     (t					; using feedmail
      (setq send-mail-function 'feedmail-send-it)
      (setq message-send-mail-function 'feedmail-send-it)))
    (setq smtpmail-debug-info t) ; I like to see the SMTP messages flash by
    (require 'smtpmail))

  (defun tkb-mail-citynet ()
    (interactive)
    (tkb-set-mail "citynet.net" "smtp.citynet.net" "tkb"))

  (defun tkb-mail-mtnet ()
    (interactive)
    (tkb-set-mail "access.mountain.net" "smtp.mountain.net"))

  ;; Obsolete.
  (defun tkb-mail-mtnet2 ()
    (interactive)
    (tkb-set-mail "mtnet2.wvnet.edu" "tkb"))

  (defun tkb-mail-wvlink ()
    (interactive)
    (tkb-set-mail "wvlink.com" "pop.mountain.net" "Kurt_Bond"))

  (defun tkb-mail-mpl ()
    "Send mail  as Kurt_Bond@mpl.com via ssh to unwind-protect.org."
    (interactive)
    (tkb-set-mail "mpl.com" "localhost" "Kurt_Bond" 6005)
    (message "Remember: ssh -L 6005:localhost:25 tkb@unwind-protect.org"))

  (defun tkb-mail-thor ()
    "Send mail  as Kurt_Bond@mpl.com via ssh to thor.mpl.com."
    (interactive)
    (tkb-set-mail "mpl.com" "localhost" "Kurt_Bond" 6005)
    (message "Remember: ssh -L 6005:localhost:25 tkb@thor.mpl.com"))

  (defun tkb-mail-tkb ()
    (interactive)
    (tkb-set-mail 
     "unwind-protect.org"
     tkb-localhost			; was "tkb.mpl.com"
     "tkb"))

  (defun tkb-mail-localhost ()
    (interactive)
    (tkb-set-mail 
     "unwind-protect.org"
     "localhost"			; was "tkb.mpl.com"
     "tkb"))

  (defun tkb-mail-ssh ()
    (interactive)
    (tkb-set-mail "unwind-protect.org" "localhost" "tkb" 6005)
    (message "Remember: ssh -L 6005:localhost:25 tkb@unwind-protect.org"))

  (defun tkb-mail-cpb ()
    (interactive)
    (tkb-set-mail "inforesrch.com" "unwind-protect" "kbond"))

  ;; What about mplvax?
  (setq tkb-localhost (or (getenv "TKB_LOCALHOST") (system-name)
			  "localhost"))

  (defun tkb-add-reply-to-tkb ()
    (mail-reply-to)
    (insert "tkb@unwind-protect.org")
    (mail-to))

  (cond ((or (string-equal tkb-localhost "tkb.mpl.com")
	     (string-equal tkb-localhost "ns1.mpl.com")
	     (string-equal tkb-localhost "erekose.mpl.com"))
	 ;; At one point I thought I'd not not actually use feedmail and
	 ;; smtpmail on tkb.mpl.com, but that messes up BCC and signature.
	 (tkb-mail-tkb))
	((or (string-match "corum" tkb-localhost)
	     (eq system-type 'darwin))
	 (tkb-mail-ssh)
	 (if (= 6005 smtpmail-smtp-service)
	     (add-hook 'mail-setup-hook 'tkb-add-reply-to-tkb)))
	(t
	 (tkb-mail-mpl)))
  )


(when nil
  ;; Switching to always using feedmail: 2006/02/13
  (not (or (string-equal tkb-localhost "tkb.mpl.com")
	   (string-equal tkb-localhost "unwind-protect.org")
	   (string-equal tkb-localhost "ns1.mpl.com")
	   (string-equal tkb-localhost "erekose.mpl.com")))
  (when (string= (upcase tkb-localhost) "KBOND")
    (tkb-mail-ssh))
  (setq send-mail-function 'feedmail-send-it)
  (setq message-send-mail-function 'feedmail-send-it)
  (autoload 'feedmail-send-it "feedmail")
  (eval-after-load "feedmail"
    '(mapc #'(lambda (directory)
	       (unless (file-directory-p directory)
		 (message "Making directory %s" directory)
		 (make-directory directory t)))
	   ;; 2008-09-04: these are set in ~/.emacs.d/custom.el, but this
	   ;; only works if done after feedmail.el is loaded.
	   (list feedmail-queue-directory feedmail-queue-draft-directory)))
  (setq feedmail-enable-queue t)
  (autoload 'feedmail-run-the-queue "feedmail")
  (autoload 'feedmail-run-the-queue-no-prompts "feedmail")
  (setq auto-mode-alist (cons '("\\.fqm$" . mail-mode) auto-mode-alist))
  (autoload 'feedmail-queue-reminder "feedmail")
  (setq feedmail-confirm-outgoing t)
  (setq feedmail-buffer-eating-function 'feedmail-buffer-to-smtpmail)
  ;; Consider (setq mail-user-agent 'message-user-agent)???
  ;;+
  ;; Note: if you've got a .mail_aliases but no aliases get defined, check
  ;; to make sure you've got a .mailrc, dummy!
  ;;-
  ;; Put at end???
  (when (string-equal (user-login-name) (user-real-login-name))
    (feedmail-queue-reminder)))




;; Mail

(when nil
  (defun tkb-found-cc-user (user-name)
    (interactive "sUser Name: ")
    (save-excursion
      (goto-char (point-min))
      (let ((cc-start (re-search-forward "^CC:" nil t)))
     (if (not cc-start)
         nil
       (let ((cc-end (re-search-forward "^[A-Za-z]" nil t)))
         (goto-char cc-start)
         (search-forward user-name cc-end t)))))))

(setq mail-aliases t)
(setq mail-personal-alias-file "~/.mail_aliases")
(setq mail-yank-prefix ">")

;;(setq mail-self-blind t)         ;automatic BCC

(when nil
  (setq mail-host-address "access.mountain.net")
  ;;(setq mail-signature t)        ;was t
  ;;(setq mail-signature '(insert "\n-- \nT. Kurt Bond, " user-mail-address))
  (setq tkb-mail-wants-system-name nil)
  (setq tkb-mail-wants-mail-host-address t)
  (defun tkb-mail-setup-hook ()
    (message "tkb-mail-setup-hook")
    (save-excursion
      (goto-char (point-min))
      (cond ((not (tkb-found-cc-user (user-login-name)))
          (mail-cc)
          (if (save-excursion
             (beginning-of-line)
             (not (looking-at "CC:[ \t]*\n")))
           (insert ", "))
          (insert (user-login-name))
          (cond (tkb-mail-wants-system-name
              (insert "@" (system-name)))
             (tkb-mail-wants-mail-host-address
              (insert "@" mail-host-address)))
          (mail-to)))))
  (add-hook 'mail-setup-hook (function tkb-mail-setup-hook)))

(when nil
  ;; This appears obsolete, since message-mode is now the default.
  (add-hook 'mail-mode-hook (function
                  (lambda ()
                    (setq paragraph-seperate
                       (concat paragraph-separate "\\|^--"))
                    (setq paragraph-start
                       (concat paragraph-start "\\|^--"))
                    (auto-fill-mode)))))

;; Rmail

;; Switched to VM, so only use that.
(unless tkb-xemacs-p
  (defadvice rmail (before tkb-no-rmail activate)
    (when (not (or executing-kbd-macro noninteractive))
      (error "You don't use rmail any more.")))
  (when nil 
    (fset 'tkb-orig-rmail (symbol-function 'rmail))
    (defun tkb-no-rmail (&rest rest)
      (interactive)
      (error "You don't use rmail any more."))
    (fset 'rmail (symbol-function 'tkb-no-rmail))

    (eval-after-load "rmail"
      '(progn (fset 'rmail (symbol-function 'tkb-no-rmail))
	      (message "rmail reset")))))



(setq rmail-file-name (expand-file-name "~/rmail/RMAIL"))
(setq rmail-default-rmail-file (expand-file-name "~/rmail/RMAIL"))
(setq rmail-secondary-file-directory (expand-file-name "~/rmail/"))

;; Here is a fun little function that, when you are positioned in an
;; RMAIL buffer, forwards the current message and all the following
;; messages to the user you specifiy.
(defun tkb-forward-lots (to)
  (interactive "sUser Name: ")
  (let ((continue t))
    (while continue
      (rmail-resend to)
      (setq continue (rmail-next-undeleted-message 1)))))

;; refile preceding messages in a new rmail file (not sure if this one works!)
(defun tkb-refile-messages (out)
  (interactive "Foutput file: ")
  (let ((continue t))
    (while continue
      (rmail-output-to-rmail-file out)
      (setq continue (rmail-delete-backward)))))

;;; Mailcrypt
;(autoload 'mc-install-write-mode "mailcrypt" nil t)
;(autoload 'mc-install-read-mode "mailcrypt" nil t)
;(add-hook 'mail-mode-hook 'mc-install-write-mode)
;(add-hook 'rmail-mode-hook 'mc-install-read-mode)
;(add-hook 'rmail-summary-mode-hook 'mc-install-read-mode)

;;; VM
(when-load-dir "vm"
  (setq vm-url-browser 'vm-mouse-send-url-to-netscape-new-window)
  (autoload 'vm "vm" "Start VM on your primary inbox." t)
  (autoload 'vm-other-frame "vm" "Like `vm' but starts in another frame." t)
  (autoload 'vm-visit-folder "vm" "Start VM on an arbitrary folder." t)
  (autoload 'vm-visit-virtual-folder "vm" "Visit a VM virtual folder." t)
  (autoload 'vm-mode "vm" "Run VM major mode on a buffer" t)
  (autoload 'vm-mail "vm" "Send a mail message using VM." t)
  (autoload 'vm-submit-bug-report "vm" "Send a bug report about VM." t)
  ;;(push-many '("image/jpeg") vm-auto-displayed-mime-content-types)
  (setq vm-auto-displayed-mime-content-types
	'("text/plain" "text" "multipart"))
  (when nil
    (setq vm-mime-external-content-types-alist
	  '(("text/html"     "nsremote")
	    ("image/gif"   "xv")
	    ("image/jpeg"  "xv")
	    ;;    ("video/mpeg"  "mpeg_play")
	    ("video"  "xanim"))))
					;(add-to-list 'vm-mime-default-face-charsets "Windows-1251")
					;(add-to-list 'vm-mime-default-face-charsets "Windows-1252")
					;(add-to-list 'vm-mime-default-face-charsets "Windows-1257")
  (setq vm-mime-default-face-charsets t)
  (setq vm-infer-mime-types t)
  (when-exec-found "base64-encode"
    (setq vm-mime-base64-encoder-program "base64-encode"))
  (when-exec-found "base64-decode"
    (setq vm-mime-base64-decoder-program "base64-decode"))
  (when-exec-found "qp-encode"
    (setq vm-mime-qp-encoder-program "qp-encode"))
  (when-exec-found "qp-decode"
    (setq vm-mime-qp-decoder-program "qp-decode"))

  ;; Why did I stop using this?  Because you have to load vm-startup so the
  ;; define-mail-user-agent call gets made.
  ;;(unless (eq mail-user-agent 'message-user-agent)
  ;;  (setq mail-user-agent 'vm-user-agent))
  (load-library "vm-startup")
  (require 'vm-autoloads)

  (setq vm-mime-8bit-text-transfer-encoding 'base64)

  (when nil 
    (defvar tkb-saved-spool-files nil)
    (defun tkb-ns1-mail ()
      (interactive)
      (if tkb-saved-spool-files
	  (message "ns1 should already be in the spool files:\n    %S"
		   vm-spool-files)
	(let ((orig-spool-files vm-spool-files))
	  (push "imap:localhost:5001:inbox:login:tkb:*" vm-spool-files)
	  (setq tkb-saved-spool-files orig-spool-files))))

    (defun tkb-reset-mail ()
      (interactive)
      (setq vm-spool-files tkb-saved-spool-files)
      (setq tkb-saved-spool-files nil)))

  (defvar tkb-vm-email-sources
    '(("citynet" .  "pop:pop3.citynet.net:110:pass:cx85276:*")
      ("vonbek"  . "imap:localhost:5001:inbox:login:tkb:*")))
   
		      
(defun tkb-vm-add ()
  (interactive)
  (let ((choices (completing-read-multiple "Sources: " tkb-vm-email-sources)))
    (mapcar (lambda (choice)
	      (let ((source (cdr (assoc choice tkb-vm-email-sources))))
		(if (not (member source vm-spool-files))
		    (push source vm-spool-files))))
	    choices))
  (message "spool files: %S" vm-spool-files))

(defun tkb-vm-remove ()
  (interactive)
  (let* ((completions
	  (mapcar (lambda (source)
		    (let ((key (car (rassoc source tkb-vm-email-sources))))
		      (cons (if key key source) source)))
		  vm-spool-files))
	 (choices (completing-read-multiple"Sources: " completions)))
    (mapcar (lambda (choice)
	      (let ((source (cdr (assoc choice completions))))
		(if (member source vm-spool-files)
		    (setq vm-spool-files (delete source vm-spool-files))
		  (message "%S is not spool file %S"
			   source vm-spool-files))))
	    choices))
  (message "spool files: %S" vm-spool-files))

  (when (string= (user-real-login-name) "root")
    (fset 'tkb-save-vm (symbol-function 'vm))
    (fset 'vm (function (lambda (&optional folder read-only access-method)
			  (interactive)
			  (message "You're root!  don't read e-mail!")))))
  )

    

;;; end of tkb-mail.el
