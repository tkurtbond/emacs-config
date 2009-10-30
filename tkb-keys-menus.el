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

(unless tkb-xemacs-p
  (define-key global-map [C-S-down-mouse-1] 'help-for-keys))


;;; Keys
(define-prefix-command 'tkb-date-map)
(global-set-key "\C-cd" 'tkb-date-map)
(define-prefix-command 'tkb-log-map)
(global-set-key "\C-cl" 'tkb-log-map)
(define-prefix-command 'tkb-map)
(global-set-key "\C-ck" 'tkb-map)	; 'k' for Kurt!
(define-prefix-command 'tkb-forms-map)
(global-set-key "\C-cF" 'tkb-forms-map)

(global-set-key [f3] 'tkb-start-categorizing-links)
(global-set-key [f4] 'tkb-categorize-next-link)
;(global-set-key [f5] 'tkb-burst-digest-and-back)
;(global-set-key [f6] 'searoom-ins-digest)
;(global-set-key [f9] 'browse-url)

(global-set-key "\C-c\C-i" 'indent-to)

(global-set-key "\C-c#" 'query-replace-regexp)

(global-set-key "\C-ckI" #'(lambda (column)
			     (interactive "P")
			     (let ((column (cond ((null column) 40)
						 ((listp column) (car column))
						 (t column))))
			       (indent-to column))))
(eval-after-load "ada-mode"
  '(define-key ada-mode-map "\C-c\M-a"
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
(global-set-key "\C-ca" 'auto-fill-mode)
(global-set-key "\C-cb" 'tkb-mail-bosses)
(global-set-key "\C-cC" 'compile)
(global-set-key "\C-cc" 'center-line)
(global-set-key "\C-cdi" 'tkb-insert-iso-date)
(global-set-key "\C-cdp" 'tkb-insert-date)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cFf" 'forms-find-file)
(global-set-key "\C-cFi" 'tkb-edit-incoming-links)
(global-set-key "\C-cFn" 'tkb-edit-new-links)
(global-set-key "\C-cFp" 'tkb-edit-phones-data)
(global-set-key "\C-cFt" 'tkb-edit-tkb-links)
(global-set-key "\C-cFw" 'tkb-edit-www-links)
(global-set-key "\C-ce" (function tkb-align-end))
(global-set-key "\C-cf" 'tkb-insert-buffer-filename)
(global-set-key "\C-ch" 'tkb-reset-isearch-highlight)
(global-set-key "\C-cI" (function tkb-insert-cut-buffer))
(global-set-key "\C-ci" 'indent-rigidly)
(global-set-key "\C-ckD" 'describe-char)
(global-set-key "\C-ckn" 'tkb-insert-name)
(global-set-key "\C-cke" 'tkb-insert-e-mail)
(tkb-keys ("\C-ckE" #'edebug-defun))
(tkb-keys ("\C-ckR" #'uncomment-region)
	  ("\C-ckr" #'comment-region))
(tkb-keys ("\C-cKU" #'tkb-unicode-character-insert))
(global-set-key "\C-clp" 'personal-log)
(global-set-key "\C-cm" 'compile)
(global-set-key "\C-cn" 'tkb-mail-self-note)
(global-set-key "\C-cN" 'tkb-mail-self-note-mpl)
(global-set-key "\C-co" 'overwrite-mode)
(global-set-key "\C-cr" 'tkb-count-region)
(global-set-key "\C-ct" 'tkb-find-next-tag)
(global-set-key "\C-cU" 'ucs-insert)
(global-set-key "\C-cu" 'tkb-insert-user-name)
(global-set-key "\C-cL" 'tkb-insert-login-name)
(global-set-key "\C-cw" 'what-line)
(global-set-key "\C-cx" 'tkb-get-clipboard)
(global-set-key "\C-cz" 'feedmail-run-the-queue)



;(define-key ctl-x-4-map "l" 'add-logbook-entry-other-window)

;; See /home/tkb/lib/emacs/cpb/personal-log.el

(eval-after-load "personal-log"
  '(progn
     (global-set-key "\C-clm" #'tkb-personal-log-insert-date-marker)
     (global-set-key "\C-clk" #'tkb-personal-log)))

;;; end of tkb-keys-menus.el
