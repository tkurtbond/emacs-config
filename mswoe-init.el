;;;; .emacs.el - for windows
;;; GNU Emacs or XEmacs?
(setq tkb-experimental-loaded-p t)
(tool-bar-mode -1)
(setq-default nxml-sexp-element-flag t)

(setq tkb-xemacs-p (string-match "xemacs" emacs-version))

(push (expand-file-name "~/local/emacs") load-path)

(load "~/lib/emacs/tkb/tkb-test.el")
(load "~/lib/emacs/tkb/tkb-macros.el")

(when-directory (d (expand-file-name "~tkb/lib/emacs"))
  (setq tkb-emacs-dir d)
  (add-to-list 'load-path tkb-emacs-dir)
  (when-directory (o (expand-file-name "~tkb/lib/emacs/tkb/others"))
    (add-to-list 'load-path o)))

(load "~/lib/emacs/tkb/tkb-functions.el")
(load "~/lib/emacs/tkb/tkb-experimental.el")
(load "~/lib/emacs/tkb/tkb-formatting.el")
(load "~/lib/emacs/tkb/tkb-time.el")
(load "~/lib/emacs/tkb/tkb-duration.el")
(load "~/lib/emacs/tkb/tkb-keys-menus.el")
(load "~/lib/emacs/tkb/tkb-lang.el")
(load "~/lib/emacs/tkb/tkb-mail.el")
(load "~/lib/emacs/tkb/tkb-text.el")
;(load "~/lib/emacs/tkb/tkb-www.el")

;; now in init.el (load "~/lib/emacs/tkb/tkb-fortune.el")


(eval-after-load "sql.el"
  '(progn 
     (push "-Usteuser" sql-postgres-options)))


(put 'sensortable 'scheme-indent-function 1)
(put 'with-slots 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)

(defun tkb-set-scheme-program ()
  (interactive)
  (let ((choices '(("MzSCheme" "c:/PROGRA~1/PLT/MzScheme.exe")
		   ("PLT-R5FS" "c:/PROGRA~1/PLT/plt-r5rs.exe")
		   ("MIT/GNU Scheme" "C:/PROGRA~1/MIT-GN~1/bin/scheme.exe --library C:/PROGRA~1/MIT-GN~1/lib")
		   ("Gambit Scheme" "c:/PROGRA~1/Gambit-C/V45~1.1/bin/gsi.exe -i")
		   ("scm" "c:/PROGRA~1/scm/scm.exe")
		   ("csi" "csi -tty-forced" ; needs my mod
		    ))))
    (setq scheme-program-name
	  (cadr (assoc (completing-read "Scheme program? "choices) choices)))))


(define-prefix-command 'tkb-date-map)
(global-set-key "\C-cd" 'tkb-date-map)
(global-set-key "\C-cdi" 'tkb-insert-iso-date)
(global-set-key "\C-cdp" 'tkb-insert-date)
(global-set-key "\C-c\t" 'indent-relative)



(add-to-list 'load-path (expand-file-name "~/lib/emacs/tkb"))
(add-to-list 'load-path (expand-file-name "~/lib/emacs/others/misc"))
;;(push "c:/emacs/emacs-21.3/share/emacs/site-lisp/w3m" load-path)

(setq common-lisp-hyperspec-root 
     "file:///c:/sw/doc/common-lisp/hyperspec/HyperSpec/")

(when nil
  ;; This seems to work ok.
  (setq scheme-program-name
	"c:/sw/versions/cygwin/chicken/2.3/bin/csi.exe -:c"))
(progn
  ;; http://groups.google.com/group/comp.lang.lisp/msg/67465d6d6423712e
  (defun lispdoc ()
    "searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
    (interactive)
    (let* ((word-at-point (word-at-point))
	   (symbol-at-point (symbol-at-point))
	   (default (symbol-name symbol-at-point))
	   (inp (read-from-minibuffer
		 (if (or word-at-point symbol-at-point)
		     (concat "Symbol (default " default "): ")
                   "Symbol (no default): "))))
      (if (and (string= inp "") (not word-at-point) (not
						     symbol-at-point))
	  (message "you didn't enter a symbol!")
        (let ((search-type (read-from-minibuffer
                            "full-text (f) or basic (b) search (default b)? ")))
          (browse-url (concat "http://lispdoc.com?q="
                              (if (string= inp "")
                                  default
				inp)
                              "&search="
                              (if (string-equal search-type "f")
                                  "full+text+search"
				"basic+search"))))))))

(when-load-file "w3m" ;; not  :load, because of the autoload
  ;; See http://cooking-with-lisp.blogspot.com/2005/07/w3m-customization.html
  ;; for good customizations

  ;; http://www.cliki.net/Bits%20from%20Mark%20Triggs's%20.emacs
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (autoload 'w3m-browse-url "w3m" "Browse web pages using w3m inside Emacs." t)
  (setq w3m-symbol 'w3m-default-symbol)
  ;;(setq w3m-key-binding 'info)

  (eval-after-load "w3m"
    '(progn
       (setq browse-url-browser-function 'w3m-browse-url)
       (define-key w3m-mode-map "T" 'w3m-view-this-url-new-session)
       ;(tkb-keys ("\C-cj" 'w3m-goto-url-new-session))
       

       ;; From http://www.cliki.net/Bits%20from%20Mark%20Triggs's%20.emacs
       (defadvice common-lisp-hyperspec (around hyperspec-lookup-w3m () activate)
	 "Browse the Common Lisp HyperSpec using w3m. When leaving w3m, restore the original window configuration."
	 (let* ((window-configuration (current-window-configuration))
		(browse-url-browser-function
		 `(lambda (url new-window)
		    (unless (member (current-buffer) (w3m-list-buffers))
		      (select-window (split-window-vertically)))
		    (w3m-browse-url url nil)
		    (let ((hs-map (copy-keymap w3m-mode-map)))
		      (define-key hs-map (kbd "q")
			(lambda ()
			  (interactive)
			  (kill-buffer nil)
			  (set-window-configuration ,window-configuration)))
		      (use-local-map hs-map))))) ad-do-it)))))

(progn
  ;; From http://www.cliki.net/Bits%20from%20Mark%20Triggs's%20.emacs ??
  (defun lisp-reindent-defun ()
    "Indent the current defun."
    (interactive)
    (save-excursion
      (beginning-of-defun)
      (indent-sexp)))

  ;; Highlight "FIXME" comments (defface fixme-face '((t (:weight bold :box (:line-width 2 :color "orange")))) "The faced used to show FIXME lines.")

  (defun show-fixme-lines (&optional arg)
    "Emphasise FIXME comments. If ARG is positive, enable highlighting. If ARG is negative, disable highlighting. Otherwise, toggle highlighting."
    (interactive)
    (if (or (and (not arg) (assoc "FIXME" hi-lock-interactive-patterns))
	    (and arg (minusp arg)))
	(unhighlight-regexp "FIXME")
      (highlight-phrase "FIXME" 'fixme-face))))


(setq
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "c:/Program Files/Mozilla Firefox/firefox.exe")

(require 'cl-lib)

(defun yank-secondary ()
  "Insert the secondary selection at point.
  Moves point to the end of the inserted text.  Does not change mark."
  (interactive) (insert (x-get-selection 'SECONDARY)))

(when-exec-found "mount.exe"
  (when (eq system-type 'windows-nt)
    (require 'cygwin-mount)
    (cygwin-mount-activate)))


(when (and nil (eq system-type 'windows-nt))
  (if (and (boundp 'Info-directory-list) Info-directory-list)
      (progn 
	(push "c:/cygwin/usr/share/info/" Info-directory-list)
	(push "c:/cygwin/usr/info/" Info-directory-list))
    (progn 
	(push "c:/cygwin/usr/share/info/" Info-default-directory-list)
	(push "c:/cygwin/usr/info/" Info-default-directory-list))))

(when (eq system-type 'windows-nt)
  ;; The first shall be last and the last shall be first.
  (if (and (boundp 'Info-directory-list) Info-directory-list)
      (progn 
	(setq Info-directory-list
	      (append Info-directory-list
		      '("c:/cygwin/usr/share/info/" "c:/cygwin/usr/info/"))))
    (progn 
      (setq Info-default-directory-list
	    (append Info-default-directory-list
		    '("c:/cygwin/usr/share/info/" "c:/cygwin/usr/info/"))))))




;; use ucs-insert to insert unicode characters.
(defun tkb-next-sexp ()
  (interactive)
  (backward-up-list -1))

(global-set-key "\C-c)" 'tkb-next-sexp)




(when nil
  (progn 
    (setq locale-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)))

(require 'cl-lib)

(fset 'tkb-select-to-record-line
   [tab ?\C-[ ?\C-f ?\C-[ ?\C-f ?% ?t ?y ?p ?e ?, ?\C-[ ?\C-k ?\C-[ ?  backspace ?\C-[ ?\C-k ?\C-[ ?m ?\C-y ?  tab ?\C-a ?\C-n])



(setq archive-zip-extract '("unzip" "-qq" "-c"))
(setq archive-zip-use-pkzip nil)
;(load-library "~/comp/tkbconfig/lib/emacs/gforth.el")
(setq forth-program-name "c:/sw/versions/cygwin/gforth/0.6.2/bin/gforth-0.6.2.exe")

;;; Connects as sysdba.
(defun tkb-oracle-sysdba ()
  (interactive)
  (let ((sql-user "sys")
        ;(sql-database "nspcp")
        (sql-oracle-options (list "as sysdba")))
    (sql-oracle)))

;(load-library "tksql.el")

(when nil
  (load-library "sql-indent.el")
  (load-library "structured-regexp.el")
  (load-library "plsql.el")
  (setq auto-mode-alist (append '(("\\.pls$"  . plsql-mode)
				  ("\\.sql$"  . plsql-mode)
				  ("\\.pks$"  . plsql-mode)
				  ("\\.pkb$"  . plsql-mode))
				auto-mode-alist)))
(when t
  (progn
    (load-library "pls-mode.el")
    (defun tkb-pls-mode-hook ()
      (interactive)
      ;;(setq tab-width 4)
      (setq indent-tabs-mode nil))
    (add-hook 'pls-mode-hook 'tkb-pls-mode-hook)

    (setq auto-mode-alist (append '(("\\.pls$"  . pls-mode)
				    ("\\.sql$"  . pls-mode)
				    ("\\.pks$"  . pls-mode)
				    ("\\.pkb$"  . pls-mode)
				    ("\\.pld$"  . diana-mode))
				  auto-mode-alist))))


;;;; Python
(when nil ;; replaced by python.el in newer versions of emacs
  (autoload 'python-mode "python-mode.el")
  (pushnew '("\\.py$" . python-mode) auto-mode-alist)
  (setq py-python-command "c:/python25/python.exe"))




;;;; SLIME Setup

(when nil 

  (setq tkb-slime-setup-done nil)
  (defun tkb-slime-setup ()
    (unless tkb-slime-setup-done
      (setq tkb-slime-setup-done t)
      (interactive)
      (setq load-path (append (list "c:/sw/versions/slime/cvs/slime")
			      load-path))
      (require 'slime)
      (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
      (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
      ;; If you don't want eldoc-like behavior, comment out the following line
      (slime-autodoc-mode)))


  ;; GNU CLISP - http://clisp.cons.org/
  (defun clisp-start-with-slime ()
    (interactive)
    (shell-command (concat "c:/sw/versions/clisp-bin/clisp-2.35/full/lisp.exe "
			   "-B c:/sw/versions/clisp-bin/clisp-2.35/full/ "
			   "-M c:/sw/versions/clisp-bin/clisp-2.35/full/lispinit.mem "
			   "-i c:/home/tkb/.slime.lisp "
			   "-ansi -q&")))

  ;; use slime-connect then read file.
  (defun tkb-clisp ()
    (interactive)
    (clisp-start-with-slime)
    (sit-for 3)
    (message "sliming?")
    (tkb-slime-setup)
    (slime-connect "127.0.0.1" 4005 t)))

(when nil				;move to tkb-experimental
  (when-directory (d "c:/sw/src/slime-from-cvs-2007-01-02/")
    (add-to-list 'load-path
		 ;;"c:/sw/versions/slime/cvs/slime/"
		 d)
    (setq inferior-lisp-program
	  ;;"c:/sw/versions/clisp-bin/local/clisp-2.35/clisp.exe"
	  ;;"c:/sw/versions/clisp-bin/local/clisp-2.38/clisp.exe -q")
	  ;;"c:/Program Files/Steel Bank Common Lisp/0.9.17/sbcl.exe")
	  ;;"c:/PROGRA~1/STEELB~1/099D9D~1.17/sbcl.exe"
	  ;;"c:/PROGRA~1/STEELB~1/1.0.2/sbcl.exe"
	  ;;"c:/PROGRA~1/STEELB~1/1.0.9/sbcl.exe"
	  "c:/PROGRA~1/STEELB~1/1.0.13/sbcl.exe"
	  )
    (setq slime-lisp-implementations
	  '((sbcl ( ;;"c:/PROGRA~1/STEELB~1/1.0.2/sbcl.exe"
		   ;;"c:/PROGRA~1/STEELB~1/1.0.13/sbcl.exe"
		   "c:/PROGRA~1/STEELB~1/1.0.29/sbcl.exe"))
	    (clisp ("c:/sw/versions/clisp-bin/clisp-2.41/clisp.exe"))
	    ;;(clisp ("clisp"))
	    (cygwin-clisp ("c:/cygwin/bin/clisp.exe"))
	    ))
    (require 'slime)
    (slime-setup)))

;;__________________________________________________________________________


(defun tkb-filename-to-registers-for-utplsql ()
  (interactive)
  (let* ((bfn (file-name-nondirectory (buffer-file-name)))
	 (bfnse (file-name-sans-extension bfn))
	 (exect (format "exec utplsql.test ('%s')" (substring bfnse 3))))
    (set-register ?f bfn)
    (set-register ?F bfnse)
    (set-register ?t exect)))

(global-set-key "\C-c#" 'query-replace-regexp)
(global-set-key "\C-cC" 'compile)
(global-set-key "\C-cf" 'tkb-insert-buffer-filename)
(global-set-key "\C-cF" 'tkb-insert-buffer-filename-sans-extension)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ct" 'tkb-filename-to-registers-for-utplsql)

(setq require-final-newline 'ask)

(global-font-lock-mode 1)

;; Font and position.


(when nil
  ;; 2009-01-17: what about this?
  (defun x ()
  (interactive)
  (let* ((font (car (x-list-fonts 
		     "-outline-DejaVu Sans Mono-normal-r-normal-normal-*-120-*-*-c-*-iso10646-1")))
	 (_ (set-frame-font font))
	 (fi (font-info font))
	 (fh (aref fi 3))
	 (dph (display-pixel-height))
	 (sh (/ (- dph 10 120) fh)))
    (set-frame-font font)
    (set-frame-height nil sh))))

(setq tkb-fonts
  '(("courier-11pt" ;; w32 says this is 11pt
     "-outline-Courier New-normal-r-normal-normal-15-*-*-*-c-*-iso10646-1"
     55)
    ("dejavu-9pt" ;; w32 says this is 9pt
     "-outline-DejaVu Sans Mono-normal-r-normal-normal-12-90-96-96-c-*-iso10646-1"
     55)
    ("dejavu-10pt" ;; w32 says this is 10pt
     "-outline-DejaVu Sans Mono-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1"
     55)
    ("dejavu-11pt" ;; w32 says this is 11pt
     "-outline-DejaVu Sans Mono-normal-r-normal-normal-15-112-96-96-c-*-iso10646-1"
     55)
    ("dejavu-13pt" ;; w32 says this is 13 point; is it iso10646-1 by default?
     "-outline-DejaVu Sans Mono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
     50)
    ;; ftp://ftp.gnu.org/pub/gnu/freefont/
    ;; http://www.gnu.org/software/freefont/
    ("freefont-14pt" ;; w32 says this is 14 point; wider than dejavu 13pt
     "-outline-FreeMono-normal-r-normal-normal-19-142-96-96-c-*-iso10646-1"
     50)
    ("freefont-13pt" ;; w32 says this is 13 point
     "-outline-FreeMono-normal-r-normal-normal-17-127-96-96-c-*-iso10646-1"
     60)
    ("unifont"
     ;; w32 doesn't show this in font selection dialog...
     ;; Windows Font Explorer shows 12 pt as best looking size.
     "-outline-unifont-medium-r-normal-normal-*-*-96-96-p-*-iso10646-1"
     65)
  
    ;; Ok if don't need unicode:
    ("dina" "-raster-Dina-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1" 60)
    ("consolas" "-outline-Consolas-normal-r-normal-normal-17-127-96-96-c-*-iso8859-1" 60)
    ("inconsolata" "-outline-Inconsolata-medium-r-normal-normal-17-127-96-96-c-*-iso8859-1" 60)
    ))

(defun tkb-set-frame-font ()
  (interactive)
  (set-frame-font (cadr (assoc-string
			 (completing-read "Font? " tkb-fonts) tkb-fonts))))
    

(defun tkb-w32-initial-font-and-size (prefix)
  (interactive "P")
  (require 'cl-lib) ;; Because I sometimes use this after emacs -q, and it needs cl.

  ;; need to do something with display-pixel-height
  (cl-destructuring-bind (tag tkb-default-font tkb-default-height)
      (assoc-string (if prefix
			(completing-read "Font? " tkb-fonts)
		      "dejavu-13pt") tkb-fonts)

    (setq tkb-default-top 20)

    (set-frame-font tkb-default-font)

    (let* ((dh (display-pixel-height))
	   (ch (frame-char-height))
	   (nfh (truncate (- (/ dh ch) (* (/ tkb-default-top ch) 2) (* (/ dh ch) .10)))))
      (when nil (not (y-or-n-p (format "dh: %d ch: %d nfh: %d " dh ch nfh)))
	    (error "not doing it"))
      ;;  FIXME: make this work if executed multiple times.
      ;;(set-frame-parameter nil 'height nfh)
      (set-frame-height nil nfh)
      (setq default-frame-alist
	    (append default-frame-alist
		    `((width . 80)
		      ;;(height . ,tkb-default-height)
		      (height . ,nfh)
		      (top . ,tkb-default-top)
		      (left . (- 50)) ;2008-07-13: -20 broke with 22.2
		      (font . ,tkb-default-font)
		      (background-color . "wheat")
		      (foreground-color . "black"))))
      (setq initial-frame-alist default-frame-alist)
      (tool-bar-mode -1)
      ;;(set-frame-parameter nil 'height tkb-default-height)
      (set-frame-parameter nil 'font tkb-default-font)
      (set-frame-parameter nil 'top tkb-default-top))))

(tkb-w32-initial-font-and-size nil)

(defvar tkb-timer nil)
(defun tkb-position-frame (&optional new-left)
  (interactive "P")
  (let* ((new-left (if new-left (prefix-numeric-value new-left) -50))
	 (dw (display-pixel-width))
	 (fw (frame-pixel-width))
	 (nl (- dw fw 50))
	 (top (frame-parameter nil 'top)))
    ;;(set-frame-position (selected-frame) nl top)
    ;;(set-frame-parameter nil 'left nl)
    ;;(frame-notice-user-settings)
    (set-frame-parameter nil 'left new-left)
    (set-frame-parameter nil 'top 20)
    (message "Yowza! %d %d %d %s" dw fw nl (current-time-string))
    (when (and tkb-timer (memq tkb-timer timer-list))
      (cancel-timer tkb-timer))))
;;(add-hook 'window-setup-hook #'tkb-position-frame)
;;(set-frame-parameter nil 'left -50)
;;(set-frame-parameter nil 'left '(- 50))
;;(set-frame-parameter nil 'top 20)
;;(set-frame-position nil -50 20)

(when nil
  (message "Setting timer: %s" (current-time-string))
  (setq tkb-timer
	(run-at-time "10 seconds" nil #'tkb-position-frame -50)))

(defun tkb-wait-frame ()
  (setq tkb-timer
	(run-at-time "10 seconds" nil #'tkb-position-frame -50)))
  

;;(add-hook 'emacs-startup-hook #'tkb-position-frame)
(add-hook 'emacs-startup-hook #'tkb-wait-frame)

(when (not (getenv "EMACS_DEFAULT_COLOR"))
  (setq default-frame-alist
	(append default-frame-alist
		'((background-color . "black")
		  (foreground-color . "white")))))


(when (and
       nil
       (not (eq system-type 'cygwin))
       (not (getenv "NO_CYGWIN_SHELL")))
  (defun my-shell-setup ()
    "For Cygwin bash under Emacs 20"
    (setq comint-scroll-show-maximum-output 'this)
    (setq comint-completion-addsuffix t)
    ;; (setq comint-process-echoes t) ;; reported that this is no longer needed
    (setq comint-eol-on-send t)
    (setq w32-quote-process-args ?\")
    (make-variable-buffer-local 'comint-completion-addsuffix))

  (setq shell-mode-hook 'my-shell-setup))

(when t
  ;; For the interactive shell
  (setq explicit-shell-file-name "c:/cygwin/bin/bash") ; remove .exe to unconfuse cmdproxy

  ;; For subprocesses invoked via the shell (e.g., "shell -c command")
  (setq shell-file-name "c:/cygwin/bin/bash"))

(setq calendar-week-start-day 1)

(setq ispell-program-name "c:/cygwin/bin/aspell.exe")

(when-load-dir (dir "psgml-1.3.2")
  (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
  (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
  (push '("\\.html" . sgml-mode) auto-mode-alist)
  (push '("\\.sgml" . sgml-mode) auto-mode-alist)
  (push '("\\.xml" . xml-mode) auto-mode-alist)
  (setq sgml-catalog-files '("c:/cygwin/usr/share/opensp/html4.soc"))
  dir)

(when nil
  (load "rng-auto.el")
  (setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	      auto-mode-alist)))

(when nil
    (setq auto-mode-alist
          (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
    (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
    (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t))

(cond-load-dir
 (["caml" "caml-mode" "ocaml-mode"]
  (setq tkb-caml-keys t)
  (if (or window-system (fboundp 'facep)) (require 'caml-font))
  (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
  (push '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
 ("tuareg-mode"
  (push "c:/emacs/emacs-21.3/site-lisp/tuareg-mode/" load-path)
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)
					;(setq tuareg-|-extra-unindent 0)
					;(setq tuareg-let-indent 2)
  (setq tuareg-in-indent 0)
  (setq tuareg-with-indent 0)
  (setq tuareg-rule-indent 0)
  (setq tuareg-parse-indent 0)
  (setq tuareg-match-indent 0)
  (setq tuareg-function-indent 0)))


(defun tkb-sql-hook ()
  (interactive)
  (setq indent-tabs-mode nil))

(add-hook 'sql-mode-hook 'tkb-sql-hook)
(add-hook 'pls-mode-hook 'tkb-sql-hook)
(add-hook 'plsql-mode-hook 'tkb-sql-hook)

(put 'narrow-to-region 'disabled nil)

(when t
  ;; c#
  ;;(when-available "c:/home/tkb/local/emacs/cc-mode-5.31" t)
  ;;(when-load-file "csharp-mode.el"
  ;;  (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
  ;;  (setq auto-mode-alist
  ;;	(append '(("\\.cs$" . csharp-mode)) auto-mode-alist)))


;;; experimental


;;; C#, csharp

  (eval-after-load "compile"
    '(setq compilation-error-regexp-alist
	   (cons '("^[ \t]*\\([[:alpha:]][-[:alnum:].]+\\)(\\([0-9]+\\),[ 	]*\\([0-9]+\\))\\(.*\\)$"
		   1			; file
		   2			; line
		   3			; column
		   )
		 compilation-error-regexp-alist)))
  (eval-after-load "csharp-mode"
    '(progn
       (defun tkb-csharp-mode-hook ()
	 (setq c-basic-offset 4)
	 (c-set-offset 'cpp-macro 0)
	 (c-set-offset 'substatement-open 0)
	 (setq show-trailing-whitespace t)
	 (setq indent-tabs-mode nil)
	 (setq indicate-empty-lines t)
	 (c-toggle-hungry-state 1)
	 )
       (add-hook 'csharp-mode-hook 'tkb-csharp-mode-hook))))

(load "~/local/emacs/nxml-mode-20041004/rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
	    auto-mode-alist))

;;; end of .emacs.el
