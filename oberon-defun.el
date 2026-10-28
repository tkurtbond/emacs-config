;;; oberon-defun.el --- PROCEDURE/MODULE motion for oberon-mode  -*- lexical-binding: t -*-

;; Load from your init file (or paste it there), replacing the earlier
;; `my-obn-beginning-of-defun', `my-obn-end-of-defun' and their hook.

(defun my-obn-beginning-of-defun (&optional arg)
  "Move to the start of a PROCEDURE heading, indented or not.
ARG > 0 moves backward ARG headings; ARG < 0 moves forward to the
start of the -ARGth next heading."
  (setq arg (or arg 1))
  (let ((origin (point)))
    (if (< arg 0)
        (progn
          (end-of-line)
          (if (re-search-forward obn-proc-heading-re nil t (- arg))
              (progn (goto-char (match-beginning 0))
                     (back-to-indentation)
                     t)
            (goto-char origin)
            nil))
      ;; Backward: a match can't extend past point, so from the start of
      ;; the line we skip this line's heading; from mid-line we include it.
      (if (looking-back "^[ \t]*" (line-beginning-position))
          (forward-line 0)
        (end-of-line))
      (if (re-search-backward obn-proc-heading-re nil t arg)
          (progn (back-to-indentation) t)
        (goto-char origin)
        nil))))

(defun my-obn--blocks ()
  "Return a list of (START HEADING-END END) for every MODULE and PROCEDURE.
START is the beginning of the heading line, HEADING-END the end of the
line holding the block's name, and END the position just after its
closing `END Name;' or `END Name.'.  Comments, strings and forward
declarations (`PROCEDURE ^') are ignored.  Blocks are in buffer order."
  (save-excursion
    (save-match-data
      (let ((case-fold-search nil)
            stack blocks)
        (goto-char (point-min))
        (while (re-search-forward "\\<\\(MODULE\\|PROCEDURE\\|END\\)\\>" nil t)
          (let ((kw (match-string-no-properties 1))
                (kw-beg (match-beginning 0))
                (kw-end (match-end 0)))
            (unless (prog1 (nth 8 (syntax-ppss kw-beg)) ; in comment/string
                      (goto-char kw-end))      ; `syntax-ppss' moves point
              (if (equal kw "END")
                  ;; `END Name' closes the innermost open block called Name.
                  (when (looking-at "[ \t]+\\(\\w+\\)")
                    (let* ((name (match-string-no-properties 1))
                           (open (seq-find (lambda (b) (equal (car b) name))
                                           stack)))
                      (when open
                        (goto-char (match-end 0))
                        (when (looking-at "[ \t]*[;.]")
                          (goto-char (match-end 0)))
                        ;; Drop any blocks left unclosed inside this one.
                        (while (not (eq (car stack) open)) (pop stack))
                        (pop stack)
                        (push (list (nth 1 open) (nth 2 open) (point)) blocks))))
                ;; A heading must be the first thing on its line; this skips
                ;; procedure types like `T = PROCEDURE (x: INTEGER)'.
                (goto-char kw-beg)
                (when (and (looking-back "^[ \t]*" (line-beginning-position))
                           (looking-at
                            (if (equal kw "MODULE")
                                "MODULE[ \t\n]+\\(\\w+\\)"
                              "PROCEDURE\\>[*-]?\\s-*\\(?:([^)]*)\\s-*\\)?\\(\\w+\\)")))
                  (push (list (match-string-no-properties 1)
                              (line-beginning-position)
                              (save-excursion (goto-char (match-end 1))
                                              (line-end-position)))
                        stack))
                (goto-char kw-end)))))
        (nreverse blocks)))))

(defun my-obn--landing (end)
  "Where `end-of-defun' leaves point for a block ending at END.
Like Emacs's default: the start of the next line if only blanks follow."
  (save-excursion
    (goto-char end)
    (skip-chars-forward " \t")
    (if (eolp) (forward-line 1) (goto-char end))
    (point)))

(defun my-obn--next-block (blocks pos)
  "Return the block whose end follows POS, or nil.
If POS is on the heading of a block, or past all nested procedures of
the innermost block around it, that is the innermost block.  Otherwise
it is the next procedure after POS."
  (let ((inner nil)
        (next nil))
    (dolist (b blocks)
      (when (and (<= (nth 0 b) pos) (< pos (nth 2 b))
                 (or (null inner) (> (nth 0 b) (nth 0 inner))))
        (setq inner b))
      (when (and (> (nth 0 b) pos)
                 (or (null next) (< (nth 0 b) (nth 0 next))))
        (setq next b)))
    (cond ((and inner (<= pos (nth 1 inner))) inner) ; on its heading
          ((and next (or (null inner) (< (nth 0 next) (nth 2 inner))))
           next)                        ; next nested/sibling procedure
          (t inner))))

(defun my-obn--next-end (blocks pos)
  "Return the landing position of the block end that follows POS, or nil.
See `my-obn--next-block' for which block that is."
  (let ((b (my-obn--next-block blocks pos)))
    (and b (my-obn--landing (nth 2 b)))))

(defun my-obn--defun-bounds ()
  "Return (START . END) of the block `my-obn-end-of-defun' would move to."
  (let ((b (or (my-obn--next-block (my-obn--blocks) (point))
               (user-error "No procedure here"))))
    (cons (nth 0 b) (my-obn--landing (nth 2 b)))))

(defun my-obn-mark-defun ()
  "Put mark at the end of the current PROCEDURE or MODULE, point at its start."
  (interactive)
  (let ((bounds (my-obn--defun-bounds)))
    (push-mark (cdr bounds) nil t)
    (goto-char (car bounds))))

(defun my-obn-narrow-to-defun ()
  "Narrow to the current PROCEDURE or MODULE."
  (interactive)
  (let ((bounds (my-obn--defun-bounds)))
    (narrow-to-region (car bounds) (cdr bounds))))

(defun my-obn-end-of-defun (&optional arg)
  "Move to the end of the current PROCEDURE or MODULE.
On a heading, go to the end of that block.  In declarations that
precede a nested procedure, go to the end of that procedure; once past
the nested procedures, go to the end of the enclosing block.  With ARG,
do it ARG times; negative ARG moves back to previous block ends."
  (interactive "^p")
  (setq arg (or arg 1))
  (let ((blocks (my-obn--blocks)))
    (if (>= arg 0)
        (dotimes (_ arg)
          (goto-char (or (my-obn--next-end blocks (point))
                         (user-error "No more procedures"))))
      (dotimes (_ (- arg))
        (let ((pos (point))
              (best nil))
          (dolist (b blocks)
            (let ((l (my-obn--landing (nth 2 b))))
              (when (and (< l pos) (or (null best) (> l best)))
                (setq best l))))
          (goto-char (or best (user-error "No previous procedures"))))))))

(defun my-obn-end-of-defun-function ()
  "`end-of-defun-function' for oberon-mode (used by `mark-defun' etc.)."
  (let ((target (my-obn--next-end (my-obn--blocks) (point))))
    (when target (goto-char target))))

(add-hook 'oberon-mode-hook
          (lambda ()
            (setq-local beginning-of-defun-function #'my-obn-beginning-of-defun)
            (setq-local end-of-defun-function #'my-obn-end-of-defun-function)))

;; Emacs's own `end-of-defun' guesses from the nearest heading above
;; point, which picks the wrong block with nested procedures, so C-M-e
;; (and C-M-h, C-x n d) use the structural version directly.
(with-eval-after-load "oberon"
  (define-key oberon-mode-map [remap end-of-defun] #'my-obn-end-of-defun)
  (define-key oberon-mode-map [remap mark-defun] #'my-obn-mark-defun)
  (define-key oberon-mode-map [remap narrow-to-defun] #'my-obn-narrow-to-defun))
