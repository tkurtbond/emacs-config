;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tkb-vc.el -- version control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-vc.el 1.1 Sun, 26 Mar 2000 15:10:50 -0500 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Backups
;(setq version-control t)

;; Version Control
(setq vc-initial-comment t)
(setq vc-mistrust-permissions t)   ;always check master file
;(setq vc-log-mode-hook (function
;              (lambda ()
;                (make-variable-buffer-local 'fill-prefix)
;                (setq fill-prefix "  ")
;                (auto-fill-mode)
;                (insert "* "))))

;;; end of tkb-vc.el
