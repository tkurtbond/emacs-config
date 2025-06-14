;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-lang.el -- programming languages and tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Debugging
;; Make `M-x dbx' do filename completion.
(defvar dbx-minibuffer-local-map nil
  "Keymap for minibuffer prompting of dbx startup command.")

(if dbx-minibuffer-local-map
    ()
  (setq dbx-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    dbx-minibuffer-local-map (kbd "C-i") 'comint-dynamic-complete-filename))


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

(when nil 
  ;; (setq open-paren-in-column-0-is-defun-start nil) ; didn't work.
  (defun tkb-beginning-of-defun-function (&optional arg)
    ;; No, it messes up end-of-defun. :-(
    (interactive)
    (re-search-backward "^[ \t]*\\s(def"))
  (setq beginning-of-defun-function #'tkb-beginning-of-defun-function)
  )

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

(push '("\\.cl\\'" . lisp-mode) auto-mode-alist)
(add-hook 'lisp-mode-hook #'(lambda () (setq indent-tabs-mode nil)))


;;(when-file (fn "~/quicklisp/slime-helper.el") (load fn))

(when nil				;not installed right now: 2015-10-16
  (when-directory (d "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/")
    (setq common-lisp-hyperspec-root (concat "file://" d))))

(when-directory (["/usr/local/share/doc/hyperspec/HyperSpec/"
                  "/home/linuxbrew/.linuxbrew/share/doc/hyperspec/HyperSpec/"])
  (eval-after-load "slime"
    '(progn
       (setq common-lisp-hyperspec-root
             "/usr/local/share/doc/hyperspec/HyperSpec/")
       (setq common-lisp-hyperspec-symbol-table
             (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
       (setq common-lisp-hyperspec-issuex-table
             (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))))

(when-file (fn "~/quicklisp/clhs-use-local.el") (load fn))

(when nil
  (when-directory (d "~/.roswell")
    (load (expand-file-name "~/.roswell/helper.el"))
    (setq inferior-lisp-program "ros -Q run")
    ))


(progn
  ;; see also the code in tkb-experimental that would set 
  ;; slime-lisp-implementations if it wasn't commented out.
  (tkb-keys
    ((kbd "C-c ;") 'slime-insert-balanced-comments)
    ((kbd "C-c M-;") 'slime-remove-balanced-comments))

  ;; Set slime-lisp-implementations and use `C-u - M-x slime' to get slime
  ;; to prompt you for one of the found implemenations by name!
  (setq slime-lisp-implementations
	`(,(when-exec-found (f ["sbcl"])
	     (message "Found an SBCL:  %s" f)
	     (setq inferior-lisp-program f) ;I guess this makes it the default
	     (let ((dir (file-name-directory f)))
	       (when dir
		 (setq dir (file-name-directory dir))
		 (let ((sbcl-info (expand-file-name "share/info/" dir)))
		   (push sbcl-info Info-default-directory-list))))
	     (list 'sbcl (list f)))
	  ,(when-exec-found (f ["clisp"])
	     (message "Found a CLISP:  %s" f)
	     (list 'clisp (list f)))
	  ,(when-exec-found (f ["ccl64"])
	     (message "Found a CCL:  %s" f)
	     (list 'ccl (list f)))
	  ,(when-exec-found (f ["ecl"])
	     (message "Found an ECL:  %s" f)
	     (list 'ecl (list f)))
	  ,(when-exec-found (f ["abcl"])
	     (message "Found an ABCL:  %s" f)
	     (list 'abcl (list f)))
	  ,(when-exec-found (f ["lisp"])
	     (message "Found an CMUCL:  %s" f)
	     (list 'cmucl (list f)))
	  ,(when-exec-found (f ["ros"])
	     (message "Found a ROSWELL: %s" f)
	     (list 'roswell (list f "-Q run")))
	  ))
  ;; Get rid of the NILs from missing implementations
  (setq slime-lisp-implementations
	(cl-loop for e in slime-lisp-implementations
	      when (not (null e)) collect e))
  (cl-loop for (name running) in slime-lisp-implementations
	do (progn (when name (princ name) (princ " ")))
	finally do (terpri)))


;; Caml
(when (require 'caml nil t)
  (require 'caml-font)
  (push '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))

;; Python
(setq python-shell-interpreter "python3")
(defun my-python-mode-hook ()
  (setq py-indent-offset 4)
  (setq font-lock-keywords python-font-lock-keywords)
  (font-lock-mode 1))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Awk
(setq awk-mode-hook
      (function (lambda ()
            (setq indent-line-function (function indent-relative)))))

;; For scheme

(defun tkb-module-scheme-indent (&rest x)
  "Since Emacs can't find defines anywhere but at the start of a line,
always indent Chicken Scheme module forms 0 characters."
  0)

(put 'syntax-rules 'scheme-indent-function 1)
(put 'transformer 'scheme-indent-function 1)
(put 'syntax-case 'scheme-indent-function 2)
(put 'call-with-values 'scheme-indent-function 1)
(put 'and-let* 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'c/output-file 'scheme-indent-function 1)
(put 'c/cwd 'scheme-indent-function 1)
(put 'sensortable 'scheme-indent-function 1)
;;(put 'module 'scheme-indent-function 2)
;;; Why does geiser/run-geiser reset this?
(put 'module 'scheme-indent-function #'tkb-module-scheme-indent)
(message "(get 'module 'scheme-indent-function) is %s\n" (get 'module 'scheme-indent-function))
(eval-after-load "geiser-repl"
  '(progn
    (defadvice geiser (after tkb-geiser-after activate)
      "Reset module's scheme-indent-function to tkb-module-scheme-indent."
      (put 'module 'scheme-indent-function #'tkb-module-scheme-indent))))

;; https://lists.gnu.org/archive/html/geiser-users/2022-12/msg00006.html
;;
;; Geiser is trying to locate your project root using Emacs's
;; project-root function.  If your directory structure is such that
;; project-root is not adequate, you can avoid it customizing
;; geiser-repl-current-project-function (e.g., you can set it to
;; #'ignore).  The REPL will then use as current directory that of the
;; first buffer from which you start it.
;; 
(setq geiser-repl-current-project-function #'ignore)

(put 'receive 'scheme-indent-function 2)
(put 'condition-case 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 1)

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

;; For MzScheme/Racket
(put 'let-values 'scheme-indent-function 'scheme-let-indent)
(put 'with-handlers 'scheme-indent-function 1)

;; For Chicken Scheme
(put 'do-list 'scheme-indent-function 2)
(put 'match-let 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'when-in-hash 'scheme-indent-function 1)
(put 'when-in-alist 'scheme-indent-function 1)
(put 'bind 'scheme-indent-function 2)
(put 'bind-loop 'scheme-indent-function 2)
(put 'submatch-named 'scheme-indent-function 1) ; For irregex SREs.

;; Hmm.  Not sure about this.  I think it is better to just always put
;; the first argument on the next line.  It's simpler and more robust.
;;(put 'args:make-option 'scheme-indent-function 3)

(when nil
  (defun tkb-beginning-of-defun (&optional arg)
    ;; This is crude, but I think it works ok.
    ;; No, it messes up end-of-defun. :-(
    (interactive "p")
    (unless arg (setq arg 1))
    (if (< arg 0)
        (dotimes (_ (- arg)) (re-search-forward "^[ \t]\\s(def"))
      (dotimes (_ arg) (re-search-backward "^[ \t]*\\s(def"))))

  (defun tkb-beginning-of-defun (&optional arg)
    (interactive "p")
    (unless arg (setq arg 1))
    (cond ((< arg 0)
           ;; Don't know what to do.
           (end-of-defun (- arg)))
          (t
           (dotimes (_ arg)
             (re-search-backward "^[ \t]*(\\(define\\|module\\)")))))


  (defun tkb-scheme-mode-hook ()
    (interactive)
    (setq-local beginning-of-defun-function #'tkb-beginning-of-defun))

  (add-hook 'scheme-mode-hook #'tkb-scheme-mode-hook)


  (when nil

    (setq beginning-of-defun-function (lambda nil 
                                        (re-search-backward "^[ \t]*(define")))
    ))

;; For Emacs lisp
(put 'eval-after-load 'lisp-indent-function 1)
(put 'global-set-key 'lisp-indent-function 1)
(put 'match 'lisp-indent-function 1) ;; for shadchen's match.

;; For Common Lisp
(put 'dotimes 'lisp-indent-function 1)
(put 'do 'lisp-indent-function 2)
(put 'block 'lisp-indent-function 1)
(put 'with-open-file 'lisp-indent-function 1)
(put 'and-let* 'lisp-indent-function 1)
(put 'when-in-hash 'lisp-indent-function 1)
(put 'register-groups-bind 'lisp-indent-function 2)
(put 'ppcre:register-groups-bind 'lisp-indent-function 2)
(put 'with-decoded-timestamp 'lisp-indent-function 2)

;; This didn't work, because there can be multiple repl buffers, so
;; you have be in one of them for it to work.
;;
;;(tkb-keys :keymap lisp-mode-map ((kbd "C-c l c") #'slime-repl-clear-buffer))

(load-library "cl-indent") ; defines the common-lisp-indent-function properties
(setq lisp-indent-function 'common-lisp-indent-function)
(cl-loop for symbol being the symbols
         for cl-indent-rule = (get symbol 'common-lisp-indent-function)
         for elisp-equivalent = (intern-soft (concat "cl-" (symbol-name symbol)))
         when (and cl-indent-rule elisp-equivalent (fboundp elisp-equivalent))
         do (put elisp-equivalent 'common-lisp-indent-function cl-indent-rule))


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
(when nil
  (setq auto-mode-alist (append '(("\\.ec\\'" . c-mode)
				  ("\\.tr\\'" . nroff-mode)
				  ("\\.nr\\'" . nroff-mode)
                                  ("\\.mom\\'" . nroff-mode)
				  ("\\.stk\\'" . scheme-mode)
				  ("\\.ss\\'" . scheme-mode)
				  ("\\.perl\\'" . perl-mode)
				  ("\\.pl\\'" . perl-mode)
				  ("\\.ph\\'" . perl-mode)
				  ("\\.pm\\'" . perl-mode)
				  )
			        auto-mode-alist)))
(cl-loop for i in '(("\\.ec\\'" . c-mode)
                 ("\\.tr\\'" . nroff-mode)
                 ("\\.nr\\'" . nroff-mode)
                 ("\\.roff\\'" . nroff-mode)
                 ("\\.mom\\'" . nroff-mode)
                 ("\\.sld\\'" . scheme-mode)
                 ("\\.stk\\'" . scheme-mode)
                 ("\\.ss\\'" . scheme-mode)
                 ("\\.perl\\'" . perl-mode)
                 ("\\.pl\\'" . perl-mode)
                 ("\\.ph\\'" . perl-mode)
                 ("\\.pm\\'" . perl-mode)
                 )
      do (add-to-list 'auto-mode-alist i)
      finally return auto-mode-alist)

(autoload 'lout-mode "lout-mode"
  "Mode for editing Lout (a typesetting language) source.")

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

(when-load-dir "~/current/goo/support"
  (message "googoogaga") (sit-for 1)
  (autoload 'goo-mode "goo" nil t)
  (push '("\\.goo\\'" . goo-mode) auto-mode-alist)
  (autoload 'run-goo "goo-shell" nil t))

(add-hook
 'js-mode-hook
 #'(lambda ()
     (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
     (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
     (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
     (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
     (define-key js-mode-map (kbd "C-c C-l") 'tkb-nodejs-repl-load-file)
     (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

(defun tkb-nodejs-repl-load-file (file)
  "Load the file to the `nodejs-repl-process'"
  (interactive (list (expand-file-name
                      (read-file-name
                       "Load file: " nil nil 'lambda
                       (file-name-nondirectory (buffer-file-name))))))
  (let ((proc (nodejs-repl--get-or-create-process)))
    (comint-send-string proc (format ".load %s\n" file))))


(cl-case 'none
  ((none))                              ;just use the default js-mode
  ((steve-yegge)
   (autoload 'js2-mode "js2" nil t)
   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  ((karl-landstrom)
   (autoload 'javascript-mode "javascript" nil t)
   (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-to-list 'auto-mode-alist '("\\.def$" . oberon-mode))
(add-to-list 'auto-mode-alist '("\\.obn$" . oberon-mode))
(add-to-list 'auto-mode-alist '("\\.Mod$" . oberon-mode))
(add-to-list 'auto-mode-alist '("\\.m$"   . oberon-mode))
(eval-after-load "oberon" '(setq oberon-indent-level 2))


(when-load-file "modula3"
  (autoload 'modula-3-mode "modula3")
  (setq auto-mode-alist
        (append '(("\\.ig$" . modula-3-mode)
                  ("\\.mg$" . modula-3-mode)
                  ("\\.i3$" . modula-3-mode)
                  ("\\.m3$" . modula-3-mode))
                auto-mode-alist)))

(dolist (d '("~/lib/emacs/others"
            "~/lib/emacs/others/misc"))
  (when-directory (o (expand-file-name d))
    (message "adding %s to load-path" o)
    (add-to-list 'load-path o)))

(when t ;; old ada-mode
  (cl-loop for d across ["~/lib/emacs/others"
                         "~/lib/emacs/others/misc"
                         ;; Using old-ada-mode right now.
                         "~/lib/emacs/others/old-ada-mode"
                         ]
        do (progn
	     (when-directory (o (expand-file-name d))
	       (message "adding %s to load-path" o)
	       (add-to-list 'load-path o))))
  (when-load-file "ada-mode"
    (message "Setting up old Ada Mode.")
    (autoload 'ada-mode "ada-mode")
    (cl-loop for ext in '("\\.gpr$" "\\.ada$" "\\.ads$" "\\.adb$")
          do (add-to-list 'auto-mode-alist (cons ext 'ada-mode)))
    (push ".ali" completion-ignored-extensions)
    (setq ada-label-indent -3)  ; I want the same level as ada-indent.
    (defun tkb-ada-mode-hook ()
      "TKB's ada-mode hook."
      (interactive)
      (message "TKB's ada-mode hook executing!")
      ;; The following doesn't seem to work.
      (setq-local comment-start "-- "))
    (add-hook 'ada-mode-hook #'tkb-ada-mode-hook)
    ))

(when nil ;; new ada-mode
  (use-package ada-mode
      :init
    (setq ada-case-strict nil)
    )
  (use-package gpr-mode)
  (use-package gpr-query))

(when nil ;; ada-ts-mode
  (defun tkb-ada-ts-mode-hook ()
    (lsp-mode))
  (use-package ada-ts-mode
      :config (progn
                (setq ada-ts-mode-indent-backend  'lsp)
                (add-hook 'ada-ts-mode-hook #'tkb-ada-ts-mode-hook))))

(when nil ;; ada-light-mode
  (when-directory (o (expand-file-name "~/lib/emacs/others/ada-light-mode"))
    (add-to-list 'load-path o)
    (message  "Loading ada-light-mode")
    (load-library "ada-light-mode")
    ))

(load-file "~/lib/emacs/emacs-config/tkb-ada.el")

(use-package
  go-mode :ensure t
  :config
  (use-package go-guru :ensure t)

  ;; (go-guru-hl-identifier-mode) ; Enable highlighting.

  ;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
                                        ;(setq gofmt-command "goimports")

  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Running M-x golint will run golint on the current file. For more
  ;; usage, see Compilation-Mode:
  ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html

  (eval-after-load 'go-mode
    '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

  (add-hook 'go-mode-hook
	    #'(lambda ()
	        ;;(flycheck-mode) ;; This is causing "suspicious state" problem.
	        (local-set-key (kbd "M-.") 'godef-jump)
	        (local-set-key (kbd "C-c C-k") 'godoc)
	        (local-set-key (kbd "C-c C-g") 'go-goto-imports)
	        (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (defun tkb-go-hook ()
    (setq indent-tabs-mode t)             ;probably go-mode does this itself.
    (setq tab-width 3))
  (add-hook 'go-mode-hook #'tkb-go-hook)
  )

(use-package php-mode)                  ; Has it really come to THIS?

;; Icon/Unicon
(setq icon-indent-level 3)              ; Traditional icon indent is 3 chars.
(setq icon-continued-statement-offset 3)
(setq icon-brace-offset 0)

;; Forth
(when t
  (when-load-file "gforth"
    (autoload 'forth-mode "gforth")
    (add-to-list 'auto-mode-alist '("\\.fs\\'" . forth-mode))
    (autoload 'forth-block-mode "gforth")
    (add-to-list 'auto-mode-alist '("\\.fb\\'" . forth-block-mode))
    (defun tkb-gforth-hook ()
      ;; Why are we setting the following to the defaults?
      ;; customize variables here:
      (setq forth-indent-level 4)
      (setq forth-minor-indent-level 2)
      (setq forth-hilight-level 3)
      ;; I hate gforth.el's indent function, and its reindent on RET.
      (setq indent-line-function #'indent-relative)
      (define-key forth-mode-map (kbd "RET") #'newline-and-indent))
    (add-hook 'forth-mode-hook #'tkb-gforth-hook)
    (setq forth-custom-indent-words
          '((("while" "[while]")
             (-2 . 2)
             (0 . 2))
            (("repeat" "[repeat]")
             (-2 . 0)
             (0 . -4))
            ))

    (eval-after-load "gforth"
      '(progn
        (load "tkb-forth")
        (defun forth-load-file (file-name)
          "Load a Forth file FILE-NAME into the inferior Forth process."
          (interactive (comint-get-source "Load Forth file: " forth-prev-l/c-dir/file
                                          forth-source-modes t)) ; T because LOAD
                                        ; needs an exact name
          (comint-check-source file-name) ; Check to see if buffer needs saved.
          (setq forth-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                              (file-name-nondirectory file-name)))
          (comint-send-string (forth-proc) (concat "include "
                                                   file-name
                                                   "\n")))))))

;; For forth mode:
(setq forth-smie-basic-indent 4)

(when-load-file "cobol-mode"
  (setq auto-mode-alist
        (append
         '(("\\.cob\\'" . cobol-mode)
           ("\\.cbl\\'" . cobol-mode)
           ("\\.cpy\\'" . cobol-mode))
         auto-mode-alist)))

;;; end of tkb-lang.el
