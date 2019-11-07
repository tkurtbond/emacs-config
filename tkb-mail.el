;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-mail.el -- mail customization and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; suppress the check that causes the sender header to be generated.
(setq message-syntax-checks '((sender . disabled)))


;;; Outgoing mail
(setq user-full-name "T. Kurt Bond")

;; Mail
(setq mail-aliases t)
(setq mail-personal-alias-file "~/.mail_aliases")
(setq mail-yank-prefix ">")


;;; end of tkb-mail.el
