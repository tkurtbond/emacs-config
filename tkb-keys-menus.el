;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-keys-menus.el -- Keys and menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-keys-menus.el 1.2 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar my-menu-map
  (make-sparse-keymap "Key Commands <==> Functions"))
     (fset 'help-for-keys my-menu-map)

(define-key my-menu-map [bindings]
  '("List all keystroke commands" . describe-bindings))
(define-key my-menu-map [key]
  '("Describe key briefly" . describe-key-briefly))
(define-key my-menu-map [key-verbose]
  '("Describe key verbose" . describe-key))
(define-key my-menu-map [function]
  '("Describe Lisp function" . describe-function))
(define-key my-menu-map [where-is]
  '("Where is this command" . where-is))

(define-key global-map [C-S-down-mouse-1] 'help-for-keys)


;;; Keys
(define-prefix-command 'tkb-date-map)
(global-set-key (kbd "C-c d") 'tkb-date-map)

(define-prefix-command 'tkb-log-map)
(global-set-key (kbd "C-c l") 'tkb-log-map)

(define-prefix-command 'tkb-map)
(global-set-key (kbd "C-c k") 'tkb-map)	; 'k' for Kurt!

(define-key tkb-map "l" (λ () (interactive) (insert ?λ)))
(define-key tkb-map "L" (λ () (interactive) (insert ?Λ)))
(define-key tkb-map " " (λ () (interactive) (insert 160))) ; non-breaking space
(define-key tkb-map "b" #'browse-url-at-point)

(define-prefix-command 'tkb-matching-map)
(global-set-key (kbd "C-c k m") 'tkb-matching-map)
(define-key tkb-matching-map "q" [?“])
(define-key tkb-matching-map "Q" [?”])
(define-key tkb-matching-map "s" [?‘])
(define-key tkb-matching-map "S" [?’])

(define-prefix-command 'tkb-forms-map)
(global-set-key (kbd "C-c F") 'tkb-forms-map)

(when nil
  (define-prefix-command 'tkb-indent-map)
  (global-set-key (kbd "C-c i") 'tkb-indent-map))

(define-prefix-command 'tkb-status-map)
(define-key tkb-map "s" 'tkb-status-map)

(define-prefix-command 'tkb-time-map)
(global-set-key (kbd "C-c T") 'tkb-time-map)

(global-set-key [f3] 'tkb-start-categorizing-links)
(global-set-key [f4] 'tkb-categorize-next-link)
;(global-set-key [f5] 'tkb-burst-digest-and-back)
;(global-set-key [f6] 'searoom-ins-digest)
;(global-set-key [f9] 'browse-url)


(global-set-key (kbd "C-c #") 'query-replace-regexp)

(global-set-key (kbd "C-c C-i") 'indent-to)
(global-set-key (kbd "C-c k I") #'(lambda (column)
			     (interactive "P")
			     (let ((column (cond ((null column) 40)
						 ((listp column) (car column))
						 (t column))))
			       (indent-to column))))
(eval-after-load "ada-mode"
  '(define-key ada-mode-map (kbd "C-c M-a")
     (function
      (lambda (column)
	(interactive "P")
	(let ((column (cond ((null column) 40)
			    ((listp column) (car column))
			    (t column))))
	  (re-search-forward "\\buse\\b")
	  (backward-word 1)
	  (indent-to column)
	  (forward-word 1))))))
(global-set-key (kbd "C-c a") 'auto-fill-mode)
(global-set-key (kbd "C-c C") 'compile)
(global-set-key (kbd "C-c c") 'center-line)

(global-set-key (kbd "C-c d i") 'tkb-insert-iso-date)
(global-set-key (kbd "C-c d I") #'tkb-kill-iso-date)
(global-set-key (kbd "C-c d f") 'tkb-insert-fancy-date)
(global-set-key (kbd "C-c d p") 'tkb-insert-date)
(global-set-key (kbd "C-c d P") #'tkb-kill-date)
(global-set-key (kbd "C-c d t") #'tkb-insert-time)

(global-set-key (kbd "C-c DEL") 'backward-delete-char-untabify)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c F f") 'forms-find-file)
(global-set-key (kbd "C-c F i") 'tkb-edit-incoming-links)
(global-set-key (kbd "C-c F n") 'tkb-edit-new-links)
(global-set-key (kbd "C-c F p") 'tkb-edit-phones-data)
(global-set-key (kbd "C-c F t") 'tkb-edit-tkb-links)
(global-set-key (kbd "C-c F w") 'tkb-edit-www-links)
(global-set-key (kbd "C-c e") (function tkb-align-end))
(global-set-key (kbd "C-c f") 'tkb-insert-buffer-filename)
(global-set-key (kbd "C-c h") 'tkb-reset-isearch-highlight)
(global-set-key (kbd "C-c I") (function tkb-insert-cut-buffer))
(global-set-key (kbd "C-c i R") 'indent-rigidly)
(global-set-key (kbd "C-c i r") 'indent-relative)
(global-set-key (kbd "C-c i t") 'indent-to)
(global-set-key (kbd "C-c k D") 'describe-char)
;;(tkb-keys ((kbd "C-c K G") #'goto-char))	;M-g c
(global-set-key (kbd "C-c k n") 'tkb-insert-name)
(global-set-key (kbd "C-c k e") 'tkb-insert-e-mail)
(tkb-keys ((kbd "C-c k E") #'edebug-defun))
(tkb-keys ((kbd "C-c k R") #'uncomment-region)
	  ((kbd "C-c k r") #'comment-region))
;;(tkb-keys ((kbd "C-c K U") #'tkb-unicode-character-insert)) ; C-x 8 RET
(global-set-key (kbd "C-c k w") 'whitespace-cleanup)
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c o") 'overwrite-mode)
(global-set-key (kbd "C-c r") 'tkb-count-region)
(global-set-key (kbd "C-c t") 'tkb-find-next-tag)
(global-set-key (kbd "C-c U") 'ucs-insert)
(global-set-key (kbd "C-c u") 'tkb-insert-user-name)
(global-set-key (kbd "C-c L") 'tkb-insert-login-name)
(global-set-key (kbd "C-c w") 'what-line)
(global-set-key (kbd "C-c x") 'tkb-get-clipboard)
(global-set-key (kbd "C-c W o") 'tkb-eww-open-file-at-point)
(defun tkb-eww-open-file-at-point ()
  (interactive) ;doesn't work
  (eww-open-file (thing-at-point 'filename t)))

(eval-after-load "rst"
  '(define-key rst-mode-map (kbd "C-c SPC")
     #'(lambda () (interactive) (ucs-insert 160 1 t))))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "<f11>") 'dired-view-file)))

;;; end of tkb-keys-menus.el
