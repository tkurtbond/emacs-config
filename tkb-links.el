;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-links.el -- functions for managing my links files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader$
;;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tkb-start-categorizing-links ()
  (interactive)
  (let ((tkb-links-file "new-links.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el"))
  (call-process "make" nil nil nil "new")
  (find-file-read-only-other-window "~/comp/tkblinks/n-links-to-cat.list")
  (let ((fb (get-buffer "new-links.el"))
     (lb (get-buffer "n-links-to-cat.list")))
    (unless fb (error "Must have a `new-links.el' buffer"))
    (unless lb (error "Must have an `n-links-to-cat.list' buffer"))
    (switch-to-buffer-other-window fb)))


(defun tkb-categorize-next-link ()
  (interactive)
  (let ((fb (get-buffer "new-links.el"))
     (lb (get-buffer "n-links-to-cat.list")))
    (unless fb (error "Must have a `new-links.el' buffer"))
    (unless lb (error "Must have an `n-links-to-cat.list' buffer"))
    (let (next)
      (save-excursion
     (set-buffer lb)
     (unless (local-variable-p 'list-pos)
       (make-local-variable 'list-pos)
       (goto-char (point-max))
       (while (and (bolp) (eolp))
         1
         (forward-line -1))
       (setq list-pos (point)))
     (when list-pos
       (goto-char list-pos))
     (unless (looking-at "^line # \\([0-9]+\\)")
       (error "Not a valid n-links-to-cat.list file?"))
     (setq next (string-to-number (match-string 1)))
     (let ((s (point))
           (e (progn (end-of-line) (point)))
           (buffer-read-only nil))
       (add-text-properties s e '(face doc-face)))
      (previous-line 1)
      (beginning-of-line)
      (setq list-pos (point)))
      (set-buffer fb)
      (forms-jump-record next))
    (forms-next-field 1)))

(defun tkb-links-browse-url ()
  (interactive)
  (when forms-file
    (let ((u (nth (1- url) forms--the-record-list)))
      (browse-url u))))
(global-set-key [f10] 'tkb-links-browse-url)

;;; end of tkb-links.el
