(defun elmo-imap4-accept-ok (session tag)
  "Accept only `OK' response from SESSION.
If response is not `OK' response, causes error with IMAP response text."
  (let ((response (elmo-imap4-read-response session tag)))
    (if (elmo-imap4-response-ok-p response)
	response
      (if (elmo-imap4-response-bye-p response)
	  (elmo-imap4-process-bye session)
	(if (elmo-imap4-response-no-p response)
	    response
	  (error "IMAP error: %s"
		 (or (elmo-imap4-response-error-text response)
		     "No `OK' response from server.")))))))


(defmacro elmo-imap4-response-no-p (response)
  "Returns non-nil if RESPONSE is an 'NO' response."
  `(assq 'no ,response))



(progn 
  (require 'mailcrypt)
  (add-hook 'wl-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'wl-mail-setup-hook 'mc-install-write-mode)

  (defun mc-wl-verify-signature ()
    (interactive)
    (save-window-excursion
      (wl-summary-jump-to-current-message)
      (mc-verify)))

  (defun mc-wl-decrypt-message ()
    (interactive)
    (save-window-excursion
      (wl-summary-jump-to-current-message)
      (let ((inhibit-read-only t))
	(mc-decrypt))))

  (eval-after-load "mailcrypt"
    '(setq mc-modes-alist
	   (append
	    (quote
	     ((wl-draft-mode (encrypt . mc-encrypt-message)
			     (sign . mc-sign-message))
	      (wl-summary-mode (decrypt . mc-wl-decrypt-message)
			       (verify . mc-wl-verify-signature))))
	    mc-modes-alist))))
