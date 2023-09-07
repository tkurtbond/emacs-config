(defvar vax-macro-mode-abbrev-table nil
  "Abbrev table for vax-macro-mode buffers.")
(define-abbrev-table 'vax-macro--mode-abbrev-table ())

(defvar vax-macro-mode-map ()
  "Keymap used in VAX MACRO mode.")
(if vax-macro-mode-map
    ()
  (setq vax-macro-mode-map (make-sparse-keymap))
  ;; No key bindings... yet.
  ;;(define-key vax-macro-mode-map "some key defn" 'some-function)
)


(defvar vax-macro-mode-syntax-table nil
  "Syntax table in use in VAX MACRO-mode buffers.")

(if vax-macro-mode-syntax-table
    ()
  (setq vax-macro-mode-syntax-table (make-syntax-table))
  ;; No syntax modifications... yet.
)

(defun vax-macro-comment-indent ()
  (if (looking-at ";;;")		;never move these
      (current-column)
    (if (looking-at ";;")		;leave alone for now
	(current-column)
      (if (looking-at "^;")
	  0				;leave alone if at beginning of line
	(skip-chars-backward " \t")	;move to bol or first nonblank
	(max (if (bolp)
		 0			;don't want extra space if blank line
	       (1+ (current-column)))	;extra space between code and comment
	     comment-column)))))

(defun vax-macro-mode ()
  "Major mode for editing VAX MACRO code.
Paragraphs are separated by blank lines only.
Commands:
\\{vax-macro-mode-map}

Turning on VAX MACRO mode calls the value of the variable vax-macro-mode-hook
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map vax-macro-mode-map)
  (setq major-mode 'vax-macro-mode)
  (setq mode-name "VAX MACRO")
  (setq local-abbrev-table vax-macro-mode-abbrev-table)
  (set-syntax-table vax-macro-mode-syntax-table)
  (make-local-variable 'page-delimiter)
  (setq page-delimiter "\\.page")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^\\([ ]*$\\|\\|" page-delimiter "\\)"))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
;;  (setq indent-line-function 'indent-relative-maybe)
  ;;(setq indent-line-function 'vax-macro-indent-relative-maybe)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'vax-macro-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode t)
  (run-hooks 'vax-macro-mode-hook))

