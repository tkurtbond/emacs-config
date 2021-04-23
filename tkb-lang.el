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
    (interactive)
    (re-search-backward "^[ \t]*\\s(def"))
  (setq beginning-of-defun-function #'tkb-beginning-of-defun-function))

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


(when-file (fn "~/quicklisp/slime-helper.el") (load fn))

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
	(loop for e in slime-lisp-implementations
	      when (not (null e)) collect e))
  (loop for (name running) in slime-lisp-implementations
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
(put 'when-in-hash 'scheme-indent-function 1)

;; For Emacs lisp
(put 'eval-after-load 'lisp-indent-function 1)
(put 'global-set-key 'lisp-indent-function 1)

;; For Common Lisp
(put 'dotimes 'lisp-indent-function 1)
(put 'do 'lisp-indent-function 2)
(put 'block 'lisp-indent-function 1)
(put 'with-open-file 'lisp-indent-function 1)
(put 'and-let* 'lisp-indent-function 1)
(put 'when-in-hash 'lisp-indent-function 1)
(put 'register-groups-bind 'lisp-indent-function 2)
(put 'ppcre:register-groups-bind 'lisp-indent-function 2)


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
(loop for i in '(("\\.ec\\'" . c-mode)
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


(case 'karl-landstrom
  ((steve-yegge)
   (autoload 'js2-mode "js2" nil t)
   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
  ((karl-landstrom)
   (autoload 'javascript-mode "javascript" nil t)
   (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
;;; end of tkb-lang.el
