;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-obsolete.el -- things I don't really use anymore.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-obsolete.el 1.1 Sun, 26 Mar 2000 15:10:50 -0500 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; load libraries
;(eval-after-load "man"
;  '(setq Man-filter-list (cons '("col -b " ()) Man-filter-list)))

;(load-library "logbook")
;(load-library "personal-log")

;(load-library "jka-compr")
;(push ["\\.tgz\\'"
;       "gzip"   "gzip"  nil  ("-c" "-q")
;       "gunzip" "gzip"  nil  ("-c" "-q" "-d")
;       t
;       nil]
;      jka-compr-compression-info-list)
;(push '("\\.tgz\\'" . tar-mode) auto-mode-alist)

;(require 'defrec)
;(autoload 'define-record "defrec" "simple mutable records for Emacs-lisp" nil
;  'macro)

;(autoload 'forms-find-file "forms" "Data entry forms for Emacs" t)

;;; end of tkb-obsolete.el
