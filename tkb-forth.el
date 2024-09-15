;;??? Not sure how this should work.
;;(defvar tkb-forth-defining-words-regexp
;;  (regexp-opt (mapcar #'s-downcase (cons ":" forth-defining-words))))

(defvar tkb-forth-defining-words-regexp
  "^\\(:\\|variable\\|create\\|struct\\)")

(defun tkb-forth-beginning-of-defun (&optional arg)
  ;;??? We're not taking care of ARG right now, but it seems ok.
  (and (re-search-backward tkb-forth-defining-words-regexp nil t arg)))

;; Don't think this does the right thing.
(defun tkb-forth-end-of-defun ()
  (if (not (looking-at tkb-forth-defining-words-regexp))
      (re-search-forward tkb-forth-defining-words-regexp nil t))
  (if (looking-at ":")
      (re-search-forward ";")
    (forward-word)))

(defun tkb-forth-mode-hook ()
  (make-local-variable 'beginning-of-defun-function)
  (setq beginning-of-defun-function #'tkb-forth-beginning-of-defun)
  (make-local-variable 'end-of-defun-function)
  (setq end-of-defun-function #'tkb-forth-end-of-defun)
  )

(add-hook 'forth-mode-hook #'tkb-forth-mode-hook)

