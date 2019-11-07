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
	    (flyspell-mode)
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

;; ConTeXt
(push '("\\.ctx\\'" . tex-mode) auto-mode-alist)

;;; end of tkb-text.el
