;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-text.el -- Text formatting and formatting languages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-text.el 1.2 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when-exec-found "aspell"
  (setq-default ispell-program-name "aspell"))

;; Plain text
(defun tkb-renumber ()
  (interactive)
  (let ((n 0))
  (while (search-forward-regexp "^[ \t]*\\([0-9]+\\)\\. +" nil t)
    (setq n (1+ n))
    (replace-match (int-to-string n) nil t nil 1))))

;; TeX
(setq tex-dvi-view-command "xdvi")


;; Nroff
(defun nroff-do-auto-fill ()
  "Don't autofill on lines that are a comment,
since the end of line is very important in [gtn]roff."
  (interactive)
  (let* ((opoint (point))
      (bol (save-excursion (beginning-of-line) (point))))
    (if (not (save-excursion (search-backward "\\\"" bol t)))
     (do-auto-fill))))

(setq nroff-mode-hook
      (function (lambda ()
            (setq comment-indent-function 'my-nroff-comment-indent)
            (auto-fill-mode)
            (make-local-variable 'auto-fill-function)
            (setq auto-fill-function (function nroff-do-auto-fill))
            (electric-nroff-mode)
            (push (cons ".(F" ".)F") nroff-brace-table)
            (push (cons ".Bn" ".En") nroff-brace-table))))

(defun my-nroff-comment-indent ()
  "Compute indent for an nroff/troff comment.
Puts a full-stop before comments on a line by themselves."
  (let ((pt (point)))
    (unwind-protect
     (progn
       (skip-chars-backward " \t")
       (if (bolp)
           (progn
          (setq pt (1+ pt))
          (setq begpos (1+ begpos))
          (insert ?.)
          1)
         (if (save-excursion
            (backward-char 1)
            (looking-at "^[.']"))
          1
           (max comment-column
             (* 8 (/ (+ (current-column)
                     9) 8)))))) ; add 9 to ensure at least two blanks
      (goto-char pt))))



;; Lout
(autoload 'lout-mode "lout-mode" "Major mode for editing Lout text." t)
(push '("\\.lout\\'" . lout-mode) auto-mode-alist)


(defun lout-comment-indent ()
  (if (looking-at "#\\+")
      (current-column)
    (if (looking-at "#-")
     (current-column)
      (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
       (current-column)
     (skip-chars-backward " \t")
     (max (if (bolp) 0 (1+ (current-column)))
          comment-column)))))


;; SGML and friends
(setq sgml-indent-data nil)
(setq sgml-indent-step nil)
(setq sgml-trace-entity-lookup nil)
(setq sgml-auto-insert-required-elements t)
(setq sgml-insert-missing-element-comment nil)
(setq sgml-validate-command "onsgmls -s %s %s")

(cond
 (t (message "Not using psgml any more"))
 ((file-loadable-p "psgml-startup")
  (message "Using FreeBSD's psgml package startup")
  (load-library "psgml-startup"))
 ((when-load-dir "psgml")
  (message "Loading psgml stuff myself")
  (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
  (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

  (push '("\\.html" . sgml-mode) auto-mode-alist)
  (push '("\\.sgml" . sgml-mode) auto-mode-alist)
  (push '("\\.xml" . xml-mode) auto-mode-alist)
  (push '("\\.dcl$" . dtd-mode) auto-mode-alist)
  (push '("\\.dec$" . dtd-mode) auto-mode-alist)
  (push '("\\.dtd$" . dtd-mode) auto-mode-alist)
  (push '("\\.ent$" . dtd-mode) auto-mode-alist)
  (push '("\\.mod$" . dtd-mode) auto-mode-alist)

  (when (file-exists-p "/usr/share/sgml/openjade-1.3.2/pubtext/HTML4.soc")
    (eval-after-load "psgml" 
      '(push "/usr/share/sgml/openjade-1.3.2/pubtext/HTML4.soc"
	     sgml-catalog-files)))

  (when (file-loadable-p "psgml-dsssl")
    (autoload 'sgml-dsssl-make-spec "psgml-dsssl" nil t))
  (when (file-loadable-p "psgml-xpointer")
    (autoload 'sgml-xpointer "psgml-xpointer" nil t))
  (when nil ;; is any of this necessary any more?
    ;; load psgml-jade extension
    ;;  (add-hook 'sgml-mode-hook '(lambda () (require 'psgml-jade)))
    (setq nsgmls-error "^nsgmls:\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):")
    (setq nsgmls-error-list (list nsgmls-error 1 2 3))
    (eval-after-load "psgml"
      '(push nsgmls-error-list sgml-validate-error-regexps))
    (setq sgml-custom-dtd
	  '(( "HTML 2.0 Strict Level 1"
	      "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0 Strict Level 1//EN\">")
	    ( "HTML 2.0 Level 1"
	      "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0 Level 1//EN\">")
	    ( "HTML 2.0"
	      "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">")
	    ( "HTML 3.2"
	      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">")
	    ( "HTML 4"
	      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">")
	    ("DocBook Book"
	     "<!DOCTYPE BOOK PUBLIC \"-//OASIS//DTD DocBook V3.1//EN\">")
	    ("DocBook Article"
	     "<!DOCTYPE ARTICLE PUBLIC \"-//OASIS//DTD DocBook V3.1//EN\">")
	    ("SDocBook Article"
	     "<!DOCTYPE article
          PUBLIC \"-//Norman Walsh//DTD Simplified DocBook XML V4.0.1//EN\"
          \"/usr/local/share/sgml/dtds/sdocbook/sdocbook.dtd\">"))))

  ;; is any of this necessary any more?
  (when (or (not (boundp 'sgml-set-face))
	    (not sgml-set-face)) 
    (setq-default sgml-set-face t)

    ;; This is from http://www.sil.org/sgml/psgml-fonts.html
    ;;
    ;; Faces.
    ;;
    (make-face 'sgml-comment-face)
    (make-face 'sgml-doctype-face)
    (make-face 'sgml-end-tag-face)
    (make-face 'sgml-entity-face)
    (make-face 'sgml-ignored-face)
    (make-face 'sgml-ms-end-face)
    (make-face 'sgml-ms-start-face)
    (make-face 'sgml-pi-face)
    (make-face 'sgml-sgml-face)
    (make-face 'sgml-short-ref-face)
    (make-face 'sgml-start-tag-face)

    (set-face-foreground 'sgml-comment-face "dark green")
    (set-face-foreground 'sgml-doctype-face "maroon")
    (set-face-foreground 'sgml-end-tag-face "blue2")
    (set-face-foreground 'sgml-entity-face "red2")
    (set-face-foreground 'sgml-ignored-face "maroon")
    (set-face-background 'sgml-ignored-face "gray90")
    (set-face-foreground 'sgml-ms-end-face "maroon")
    (set-face-foreground 'sgml-ms-start-face "maroon")
    (set-face-foreground 'sgml-pi-face "maroon")
    (set-face-foreground 'sgml-sgml-face "maroon")
    (set-face-foreground 'sgml-short-ref-face "goldenrod")
    (set-face-foreground 'sgml-start-tag-face "blue2")

    (setq-default sgml-markup-faces
		  '((comment . sgml-comment-face)
		    (doctype . sgml-doctype-face)
		    (end-tag . sgml-end-tag-face)
		    (entity . sgml-entity-face)
		    (ignored . sgml-ignored-face)
		    (ms-end . sgml-ms-end-face)
		    (ms-start . sgml-ms-start-face)
		    (pi . sgml-pi-face)
		    (sgml . sgml-sgml-face)
		    (short-ref . sgml-short-ref-face)
		    (start-tag . sgml-start-tag-face)))))
  (t
   (message "warning: psgml not available")))

(when-load-dir "tdtd"
  ;; DTD mode
  (autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
  (autoload 'dtd-etags "tdtd"
    "Execute etags on FILESPEC and match on DTD-specific regular expressions."
    t)
  (autoload 'dtd-grep "tdtd" "Grep for PATTERN in files matching FILESPEC." t)

  ;; Turn on font lock when in DTD mode
  (add-hook 'dtd-mode-hooks
    'turn-on-font-lock)

  (setq auto-mode-alist
	(append
	 (list
	  '("\\.dcl$" . dtd-mode)
	  '("\\.dec$" . dtd-mode)
	  '("\\.dtd$" . dtd-mode)
	  '("\\.ele$" . dtd-mode)
	  '("\\.ent$" . dtd-mode)
	  '("\\.mod$" . dtd-mode))
	 auto-mode-alist))
  'loaded)


(when nil
  (setq sgml-xml-declaration "C:/jade/xml.dcl")
  (setq sgml-shell "c:/emacs/emacs-20.3.1/bin/cmdproxy.exe")
  (setq sgml-command-list
	(list  (list "Jade" "jade -c%catalogs -t%backend -d%stylesheet %file" 
		     'sgml-run-command nil
		     '(("jade:\\(.*\\):\\(.*\\):\\(.*\\):E:" 1 2 3)))
	       (list "JadeTeX" "tex \"&jadetex\" %tex" 
		     'sgml-run-command nil)
	       (list "JadeTeX PDF" "virpdftex \"&pdfjadetex\" %tex"
		     'sgml-run-command t)
	       (list "dvips" "dvips %dvi"
		     'sgml-run-command nil)
	       (list "View dvi" "yap %dvi" 
		     'sgml-run-background t)
	       (list "View PDF" "gsview32 %pdf" 
		     'sgml-run-background t)
	       (list "View ps" "gsview32 %ps"
		     'sgml-run-command nil)))

  (setq sgml-expand-list
	(list 
	 (list "%file" 'file nil)	; the current file as is
	 (list "%sgml" 'file "sgml"
	       ;;sgml-sgml-file-extension
	       )			;   with given extension
	 (list "%tex" 'file "tex")	;   dito 
	 (list "%dvi" 'file "dvi")	;   dito
	 (list "%pdf" 'file "pdf")	;   dito
	 (list "%ps" 'file "ps")	;   dito
	 (list "%dsssl" 'file "dsl"
	       ;;sgml-dsssl-file-extension
	       )			;   dito
	 (list "%dir" 'file nil t)	; the directory part  
	 (list "%stylesheet" 'sgml-dsssl-spec) ; the specified style sheet
	 (list "%backend" 'sgml-jade-backend) ; the selected backend
	 (list "%catalogs" 'sgml-dsssl-catalogs 'sgml-catalog-files 'sgml-local-catalogs)
	 ;; the catalogs listed in sgml-catalog-files and sgml-local-catalogs.
	 ))
  ;; load psgml-jade extension
  ;;(add-hook 'sgml-mode-hook '(lambda () (require 'psgml-jade)))
  ;; load dsssl support
  ;;(autoload 'sgml-dsssl-make-spec "psgml-dsssl" nil t)
  )

;;; end of tkb-text.el
