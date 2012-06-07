;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-lang.el -- programming languages and tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-lang.el 1.2 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debugging
;; Make `M-x dbx' do filename completion.
(defvar dbx-minibuffer-local-map nil
  "Keymap for minibuffer prompting of dbx startup command.")

(if dbx-minibuffer-local-map
    ()
  (setq dbx-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    dbx-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))


(defadvice dbx (before with-completion first activate)
  "Run dbx with completion."
  (interactive
   (list (read-from-minibuffer "Run dbx (like this): "
                      (if (consp gud-dbx-history)
                       (car gud-dbx-history)
                     "dbx ")
                      dbx-minibuffer-local-map nil
                      '(gud-dbx-history . 1)))))


;; Lisp

;; I found these useful when working with the meta parsing technique in
;; Common Lisp.
(defun tkb-syn ()
  (interactive)
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){"))

(defadvice lisp-mode (after tkb-lisp-mode-adv first activate)
  (tkb-syn))

(when nil
  (progn (require 'slime)
	 (slime-setup)))


;; Caml
(cond-load-dir
 (["caml" "caml-mode" "ocaml-mode"]
  (setq tkb-caml-keys t)
  (if (or window-system (fboundp 'facep)) (require 'caml-font))
  (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
  (push '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
 ("tuareg-mode"
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
  (setq tuareg-in-indent 0)))


;; Python
(defun my-python-mode-hook ()
  (setq py-indent-offset 4)
  (setq font-lock-keywords python-font-lock-keywords)
  (font-lock-mode 1))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Awk
(setq awk-mode-hook
      (function (lambda ()
            (setq indent-line-function (function indent-relative)))))


;; Perl
(defun my-perl-comment-indent ()
  (if (and (bolp) (not (eolp)))
      0					 ; Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))         ; Else indent at comment
	   column
	   comment-column))))		     ; except leave at least one space.


;; For scheme
(put 'syntax-rules 'scheme-indent-function 1)
(put 'transformer 'scheme-indent-function 1)
(put 'syntax-case 'scheme-indent-function 2)
(put 'call-with-values 'scheme-indent-function 1)
(put 'and-let* 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'c/output-file 'scheme-indent-function 1)
(put 'c/cwd 'scheme-indent-function 1)
(put 'sensortable 'scheme-indent-function 1)
(put 'module 'scheme-indent-function 2)

;; For scheme code from 3imp
(put 'rec 'scheme-indent-function 1)
(put 'recur 'scheme-indent-function 2)
(put 'record 'scheme-indent-function 2)
(put 'record-case 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)

;; For scsh
(put 'with-cwd 'scheme-indent-function 1)

;; For Unroff code
(put 'with-output-to-stream 'scheme-indent-function 1)
;; For MzScheme
(put 'let-values 'scheme-indent-function 'scheme-let-indent)
(put 'with-handlers 'scheme-indent-function 1)

;; For Emacs lisp
(put 'eval-after-load 'lisp-indent-function 1)
(put 'global-set-key 'lisp-indent-function 1)

;; For Common Lisp
(put 'dotimes 'lisp-indent-function 1)
(put 'do 'lisp-indent-function 2)
(put 'block 'lisp-indent-function 1)
(put 'with-open-file 'lisp-indent-function 1)

(put 'and-let* 'lisp-indent-function 1)

;; For EuLisp
(put 'dynamic-let 'lisp-indent-function 1)
(put 'named-lambda 'lisp-indent-function 'defun)
(put 'inlined-lambda 'lisp-indent-function 'defun)
(put 'generic-lambda 'lisp-indent-function 'defun)
(put 'let/cc 'lisp-indent-function 1)
(put 'with-handler 'lisp-indent-function 1)

;; For C
;(setq c-tab-always-indent nil)


;; Loading
(setq auto-mode-alist (append '(("\\.txi\\'" . txi-mode)
				("\\.ec\\'" . c-mode)
				("\\.tr\\'" . nroff-mode)
				("\\.nr\\'" . nroff-mode)
				;;("\\.py\\'" . python-mode)
				("\\.stk\\'" . scheme-mode)
				("\\.ss\\'" . scheme-mode)
				("\\.perl\\'" . perl-mode)
				("\\.pl\\'" . perl-mode)
				("\\.ph\\'" . perl-mode)
				("\\.pm\\'" . perl-mode)
				)
			      auto-mode-alist))


;; Eval-after options

(when nil 
  (when-exec-found (exe '("cmucl" "clisp")
			(cons "/sw/test/cmucl/bin" exec-path))
    (setq inferior-lisp-program exe)))


(when-exec-found (exe '("youtoo") (append '("/sw/src/EuLysses"
					    "/sw/src/u2/EuLysses")
					  exec-path))
  (setq inferior-youtoo-program exe)
  (push '("\\.em\\'" . lisp-mode) auto-mode-alist))

(defun run-youtoo (prompt)
  (interactive "P")
  (let ((inferior-lisp-program
	 (if prompt
	     (setq inferior-youtoo-program (read-file-name "Youtoo program: "))
	   inferior-youtoo-program)))
    (run-lisp inferior-lisp-program)))

;; What combinations of scheme systems and slib macros should we use?
(eval-after-load "cmuscheme"
  '(cond
    (t
     (when-exec-found (exe "csi") (setq scheme-program-name exe)))
    (nil
     (when-exec-found (exe '("/sw/test/bigloo/bin/bigloo" "bigloo" "scsh" "scheme48" "mzscheme" "scm"))
       (setq scheme-program-name exe)))
    (nil
     '(setq scheme-program-name "mzscheme"))
    (nil
     '(setq scheme-program-name "scheme48"))
    (nil
     ;; bare.
     '(setq scheme-program-name "scm"))
    (nil
     (setq scheme-program-name "stk"))
    (nil
     ;; Faster (loading) than syntax-case, slower than macros-that-work,
     ;; but has more essential special forms than macros that work.
     '(setq scheme-program-name
	    "scm -rsyntactic-closures -rrepl -rstruct -l/home/tkb/MyInit.scm"))
    (nil
     ;; slower (loading) than syntactic-closures, more complete than
     ;; macros-that-work, has better low-level macros than
     ;; syntactic-closures.
     '(setq scheme-program-name
	    "scm -rsyntax-case -rrepl -m"))
    (nil
     ;; Not as complete as syntax-case or syntatic-closures, but much faster
     ;; (at least to load)
     '(setq scheme-program-name
	    "scm -rmacros-that-work -rrepl -l/home/tkb/MyInit.scm"))))
(autoload 'run-scheme "cmuscheme" "Better scheme subprocess mode" t)

(autoload 'psd-mode "psd"
  "Minor mode for running psd (the Portable Scheme Debugger) in a cmuscheme
buffer."
  t)

;; Newer emacen have python.el.
;;(autoload 'python "python-mode"
;;  "Mode for editing and running python." t)

;(autoload 'lout-mode "lout"
;  "Mode for editing Lout (a typesetting language) source.")

(autoload 'lout-mode "lout-mode"
  "Mode for editing Lout (a typesetting language) source.")

;; Erlang
(when-load-dir "erlang"
  (setq erlang-root-dir "/usr/local/erlang")
  (setq exec-path (cons "/usr/local/erlang/bin" exec-path))
  
  (require 'erlang-start))


(autoload 'uil-mode "uil-mode" "Major mode for editing uil files" t)
(set-variable 'auto-mode-alist
           (append '(("\\.uil$" . uil-mode)) auto-mode-alist))
(setq completion-ignored-extensions
      (cons ".uid" completion-ignored-extensions))

(cond (nil
       (autoload 'sql "sql-mode"
	 "Start the interactive SQL interpreter in a new buffer." t)

       (autoload 'sql-mode "sql-mode"
	 "Mode for editing SQL files and running a SQL interpetror." t)

       (setq auto-mode-alist (cons '("\\.sql$" . sql-mode) auto-mode-alist)))
      (nil
       (require 'sql)
       (defalias 'sql-get-login 'ignore))
      (nil
       (tkb-dbg (tkb-show-abbrev-settings))
       (load-library "sql")
       (setq abbrevs-changed nil)
       (tkb-dbg (tkb-show-abbrev-settings))
       ))

;;; Connects as sysdba.
(defun tkb-oracle ()
  (interactive)
  (let ((sql-user "sys")
        ;(sql-database "nspcp")
        (sql-oracle-options (list "as sysdba")))
    (sql-oracle)))


;;???
(defun tkb-gupta-outline-level ()
  (interactive)
  (looking-at "\\.head[ \t]+\\([0-9]+\\)")
  (1+ (string-to-int (buffer-substring (match-beginning 1) (match-end 1)))))


;; Mostly of historical interest, except when I get dragged back into
;; POISE Hell.
(setq auto-mode-alist
      (append '(("\\.bas\\'" . basic-mode)
		("\\.bas@.+" . basic-mode)
		("\\.BAS\\'" . basic-mode)
		("\\.BAS@.+" . basic-mode))
	      auto-mode-alist))
(autoload 'basic-mode "vaxbasic"
  "Simplistic Mode for editing VAX Basic programs." t)

(when-load-dir "/usr/local/lib/dylan/2.3.9/x86-freebsd-elf-gcc/elisp"
  (autoload 'dylan-mode "dylan-mode")
  (push '("\\.dylan\\'" . dylan-mode) auto-mode-alist))

(when-load-dir "~/current/goo/support"
  (message "googoogaga") (sit-for 1)
  (autoload 'goo-mode "goo" nil t)
  (push '("\\.goo\\'" . goo-mode) auto-mode-alist)
  (autoload 'run-goo "goo-shell" nil t))

(when-load-dir "haskell-mode"
  (setq auto-mode-alist
	(append auto-mode-alist
		'(("\\.[hg]s$"  . haskell-mode)
		  ("\\.hi$"     . haskell-mode)
		  ("\\.l[hg]s$" . literate-haskell-mode))))
  (autoload 'haskell-mode "haskell-mode"
    "Major mode for editing Haskell scripts." t)
  (autoload 'literate-haskell-mode "haskell-mode"
    "Major mode for editing literate Haskell scripts." t)

  ;; Add the following lines according to which modules you want to use:

  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (add-hook 'haskell-mode-hook 'turn-on-font-lock))

(case 'karl-landstrom
  ((steve-yegge)
   (autoload 'js2-mode "js2" nil t)
   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  ((karl-landstrom)
   (autoload 'javascript-mode "javascript" nil t)
   (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))))
     

(when-load-file (f "o2-default.el") :load 
  )

(when-load-file (f "graphviz-dot-mode.el")
  (autoload 'graphviz-dot-mode f nil t)
  (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode)))
;;; end of tkb-lang.el
