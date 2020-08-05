;;; BASIC_MODE.EL -- mode for editing BASIC program source.
;;; A Work In Progress; Very Crude.
;;; TAB runs indent-relative.  

;;; This was probably written sometime in the very early 1990s.
;;; Who would have thought I'd be modifying it in 2003?

(defvar basic-mode-abbrev-table nil
  "Abbrev table in use in Basic-mode buffers.")
(define-abbrev-table 'basic-mode-abbrev-table ())

(defvar basic-mode-map ()
  "Keymap used in Basic mode.")
(if basic-mode-map
    ()
  (setq basic-mode-map (make-sparse-keymap))
  ;; GNU Emacs on VMS 5.5-2 is probably too old for the kbd function.
  (define-key basic-mode-map "\e\C-a" 'beginning-of-basic-defun) 
  (define-key basic-mode-map "\e\C-e" 'end-of-basic-defun)
  (define-key basic-mode-map "\C-cs" 'basic-back-to-label)

  (define-key basic-mode-map "\C-cB" 'mark-basic-function)
  (define-key basic-mode-map "\177" 'backward-delete-char-untabify)
  (define-key basic-mode-map "\t" 'indent-relative))

(defvar basic-mode-syntax-table nil
  "Syntax table in use in Basic-mode buffers.")

(if basic-mode-syntax-table
    ()
  (setq basic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" basic-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" basic-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" basic-mode-syntax-table)
  (modify-syntax-entry ?+ "." basic-mode-syntax-table)
  (modify-syntax-entry ?- "." basic-mode-syntax-table)
  (modify-syntax-entry ?= "." basic-mode-syntax-table)
  (modify-syntax-entry ?% "." basic-mode-syntax-table)
  (modify-syntax-entry ?< "." basic-mode-syntax-table)
  (modify-syntax-entry ?> "." basic-mode-syntax-table)
  (modify-syntax-entry ?& "." basic-mode-syntax-table)
  (modify-syntax-entry ?\| "." basic-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" basic-mode-syntax-table))

(defconst basic-indent-level 4
  "*Indentation of Basic statements with respect to containing block.")
(defconst basic-label-offset -4
  "*Offset of Basic label lines and case statements relative to
usual indentation.")
(defconst basic-continued-statement-offset 4
  "*Extra indent for lines not starting new statements.")

(defconst basic-continued-line-pat "[&]"
  "*Regexp matching characters that indicate a continued line.")

(defvar basic-mode-hook nil
  "Hook for entering basic mode.")


(defun basic-mode ()
  "Major mode for editing Basic code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Commands:
\\{basic-mode-map}

Turning on Basic mode calls the value of the variable basic-mode-hook
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map basic-mode-map)
  (setq major-mode 'basic-mode)
  (setq mode-name "Basic")
  (setq local-abbrev-table basic-mode-abbrev-table)
  (set-syntax-table basic-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ ]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'indent-relative-maybe)
  (setq indent-line-function 'basic-indent-relative-maybe)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "!")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "!+[ \t]*")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'basic-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'basic-mode-hook))

;; This is used by indent-for-comment
;; to decide how to indent a comment in Basic code
;; based on its context.

(defun basic-comment-indent ()
  (if (looking-at "!\\+")
      (current-column)
    (if (looking-at "!-")
	(current-column)
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "^[ \t]*!"))
	  (current-column)		;leave alone if at beginning of line
	(skip-chars-backward " \t")	
	(max (if (bolp) 0 (1+ (current-column)))
	     comment-column)))))



(defun basic-indent-relative-maybe ()
  "Indent a new line like previous nonblank line."
  (interactive)
  (indent-relative t)
  (let ((i (save-excursion
	  (beginning-of-line)
	  (if (prev-basic-line 1)
	      ;;;
	      (cond 
	       ((basic-continued-line)
		(if (prev-basic-line 1)
		    (if (not (basic-continued-line))
			'do-indent)
		  'do-indent))
	       ((progn
		  (skip-chars-forward "^!\n") ;!was#
		  (skip-chars-backward " \t")
		  (backward-char 1)
		  (looking-at "{"))
		'do-indent)
		;;last line not continued on new line  See if it was continued
		(t
		 (beginning-of-line)
		 (if (prev-basic-line 1)
		     (if (basic-continued-line)
			 'do-undent
		       'no-indent)
		   'no-indent)))	       
	       'no-indent))))
    (cond
     ((eq i 'do-indent)
      (insert-char ?\  basic-indent-level))
     ((eq i 'do-undent)
      (basic-delete-back basic-indent-level)))))



(defun prev-basic-line (n)
  "Move to previous Basic line"
  (re-search-backward "^[^\n]" nil t))



(defun basic-continued-line ()
  "Return t if line ends with a continuation character"
  (save-excursion
    (skip-chars-forward "^\n")
    (skip-chars-backward " \t")
    (backward-char 1)
    (looking-at basic-continued-line-pat)))
	
    

(defun basic-delete-back (arg)
  (while (and (> arg 0)
	      (or (= (preceding-char) ?\ )
		  (= (preceding-char) ?\t)))
    (backward-delete-char-untabify 1)
    (setq arg (1- arg))))

(defun beginning-of-basic-defun ()
  "Go to the start of the enclosing procedure; return t if at top level."
  (interactive)
  (if (re-search-backward "^\\(function\\|sub\\|program\\)\\s \\|^end[ \t\n]"
			  (point-min) 'move)
      (looking-at "e")
    t))


(defun end-of-basic-defun ()
  "Go to the end of the enclosing function."
  (interactive)
  (if (not (bobp)) (forward-char -1))
  (re-search-forward
   "\\(\\s \\|^\\)end[ 	]+\\(function\\|sub\\|program\\)\\(\\s \\|$\\)"
   (point-max) 'move)
  (forward-word -1)
  (forward-line 1))


(defun mark-basic-function ()
  "Put mark at end of BASIC function, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-basic-defun)
  (push-mark (point))
  (beginning-of-line 0)
  (beginning-of-basic-defun))


(defun basic-mode-default-hook ()
  (abbrev-mode 1))

(add-hook 'basic-mode-hook (function basic-mode-default-hook))


(defun basic-back-to-label ()
  "Move backward to the previous BASIC label, hoping it is the one that
started the current subroutine."
  (interactive)
  (push-mark)
  (re-search-backward "^[ \t]*[0-9]*[ \t]*[.$a-z_0-9]+[ \t]*:[^:]"))
