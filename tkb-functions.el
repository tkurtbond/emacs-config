;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-functions.el -- these are functions that are generic emacs functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-functions.el 1.2 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun y-or-n-p/timeout (prompt &optional timeout default)
  "Ask user a \"y or n\" question, but after TIMEOUT seconds return DEFAULT,
which should be `t' or `nil'.  TIMEOUT defaults to 5 seconds."
  (interactive)
  (unless timeout (setq timeout 5.0))
  (with-timeout (timeout
                 (message "%s timed out after %f seconds; returning %S"
                            prompt timeout default)
                 default)
    (y-or-n-p prompt)))

(defun empty-string-p (s)
  "Return true if S is empty"
  (zerop (length s)))

(defvar tkb-align-column 79
  "Put the last character of this line in `tkb-align-column'")

(defun tkb-align-end (col)
  "Align the end of this line at character `tkb-align-column',
or at the column specified by the prefix arg."
  (interactive "P")
  (let ((col (if col col tkb-align-column)))
    (let ((current-end (save-excursion
                         (end-of-line)
                         (current-column))))
      (if (< current-end col)
          (insert (make-string (- col current-end) ? ))))))

(defun tkb-get-clipboard ()
  (interactive)
  (insert (x-get-clipboard)))


(defun tkb-show-position ()
  (interactive)
  (let* ((params (frame-parameters))
       (top (assq 'top params))
       (left (assq 'left params))
       (height (assq 'height params))
       (width (assq 'width params)))
    (message "(%s,%s) %sx%s" top left height width)))


(defun tkb-insert-buffer-filename (prefix)
  "Insert the filename part of the buffer filename (or the buffer
name, if the buffer does not have a filename), or the whole
filename if a prefix greater than 1 is specified (remember C-u by
itself is 4), or the filename part without an extension if the
prefix is less than 0."
  (interactive "p")
  (let ((name (or (buffer-file-name) (buffer-name))))
    (insert (if (> prefix 1)
                name
              (if (< prefix 0)
                  (file-name-sans-extension
                   (file-name-nondirectory name))
                (file-name-nondirectory name))))))

(defun tkb-count-region ()
  (interactive)
  (message "%d characters in region."
           (abs (- (mark) (point)))))

(defun tkb-pad-string (s)
  "Pad string S with spaces between each character"
  (interactive "sString: ")
  (let ((l (length s)) (s2 (char-to-string (aref s 0))) (i 1))
    (while (< i l)
      (setq s2 (concat s2 " " (char-to-string (aref s i))))
      (setq i (1+ i)))
    (insert s2)))

(defun tkb-ibn-padded (up)
  "Insert Buffer Name with spaces between each character. With arg, upcase."
  (interactive "P")
  (tkb-pad-string (cond (up (upcase (buffer-name)))
                        (t (buffer-name)))))

(defun tkb-ibn (up)
  "Insert Buffer Name. With arg, upcase."
  (interactive "P")
  (insert (cond (up (upcase (buffer-name)))
                (t (buffer-name)))))
(tkb-keys ((kbd "C-c k B") #'tkb-ibn))

(defun tkb-stretch-string (s)
  (interactive "sString: ")
  (let* ((len (length s))
         (ns (make-string (* len 3) ?\s))
         (i 0))
    (while (< i len)
      (let ((c (aref s i))
            (n (* i 3)))
        (aset ns n c)
        (aset ns (+ n 1) c)
        (aset ns (+ n 2) c))
      (incf i))
    (insert ns)))


(defun tkb-insert-user-name ()
  (interactive)
  (insert (user-full-name)))

(defun tkb-insert-login-name ()
  (interactive)
  (insert (user-login-name)))

(defun tkb-find-next-tag ()
  (interactive)
  (find-tag nil t nil))

(defun tkb-mail-self-note (self)
  (interactive "P")
  (message "mail-self: %S" self)
  (let ((to (if self
                (if (stringp self)
                    self
                  (completing-read "Username: "
                                   `(("tkurtbond+note@gmail.com" 1)
                                     ("kbond+note@mpl.com" 2))))
              "tkurtbond+note@gmail.com")))
    ;; Whoa, way too fiddly. ???
    (let ((mail-self-blind (and self
                                (or (and (numberp self)
                                         (< self 0))
                                    (eq self '-)))))
      (compose-mail to)
      (mail-subject))))
(global-set-key (kbd "C-c m n") 'tkb-mail-self-note)

(defun tkb-mail-self-note-mpl ()
  (interactive)
  (tkb-mail-self-note "kbond+note@mpl.com"))
(global-set-key (kbd "C-c m N") 'tkb-mail-self-note-mpl)

(defun tkb-mail-bosses ()
  (interactive)
  (random t)
  (let ((bosses ["htheiling@mpl.com" "dsweda@mpl.com" "cbenson@mpl.com"
                 "bmoats@mpl.com"])
        to)
    (while (not (= (length to) 3))
      (let ((user (aref bosses (random 3))))
        (unless (member user to)
          (push user to))))
    (message "To: %S" to)
    (compose-mail (mapconcat (function (lambda (n) n)) to ", "))
    (mail-subject)))
(global-set-key (kbd "C-c m b") 'tkb-mail-bosses)

(defun tkb-edit-incoming-links ()
  (interactive)
  (let ((tkb-links-file "Incoming/incoming.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-new-links ()
  (interactive)
  (let ((tkb-links-file "new-links.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-tkb-links ()
  (interactive)
  (let ((tkb-links-file "tkb-links.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-www-links ()
  (interactive)
  (let ((tkb-links-file "www-links.data"))
    (forms-find-file "~/comp/tkblinks/new-links.el")))

(defun tkb-edit-phones-data ()
  (interactive)
  (forms-find-file "~/tkb/Data/Phones/phones.fmt"))


(defun tkb-insert-cut-buffer ()
  (interactive)
  (insert (x-get-cut-buffer)))

(defun tkb-reset-isearch-highlight ()
  (interactive)
  (setq isearch-lazy-highlight (not isearch-lazy-highlight)))

(cl-defun tkb-check-bindings (keys-list &optional bindings-list (keymap global-map))
  "Check if the key sequences in KEYS-LIST are already defined;
BINDINGS-LIST optionally contains the new bindings (functions)."
  (unless bindings-list
    (setq bindings-list (make-list (length keys-list) nil)))
  (cl-loop for keys in keys-list
        for new-binding in bindings-list
        for binding = (lookup-key keymap keys)
        ;; lookup-key returns a number for key sequences that don't have
        ;; valid sequence of prefix characters in the keymap.
        when (and binding (not (numberp binding)))
        collect (cons keys binding)
        into bindings
        and collect (if new-binding
                        (format "  %s is %S but will become %S\n" (key-description keys) binding new-binding)
                      (format "  %s is %S\n" (key-description keys) binding))
        into msgs
        finally return (cl-values bindings (apply #'concat msgs))))

(cl-defun tkb-key-is-bound-to (key expected-fun &optional (keymap global-map))
  "Check if the key sequence in KEY is bound to EXPECTED-FUN."
  (let ((fun (lookup-key keymap key)))
    (cond
     ((equal fun expected-fun)
      (message "Key sequence '%s' is '%s' as expected" (key-description key)
               expected-fun))
     (t
      (error "Key sequence '%s' is '%s' not '%s'" (key-description key)
             fun expected-fun)))))

(when nil
  (tkb-check-bindings (list (kbd "C-c k R")))
  (tkb-check-bindings (list (kbd "C-c k r")))
  (tkb-keys ((kbd "C-c k P") #'ding))
  (tkb-check-bindings (list (kbd "C-c k p")))
  )

(defun tkb-rfc822-date-time (&optional time)
  ;; http://www.faqs.org/rfcs/rfc822.html
  (format-time-string "%a, %e %b %y %T %Z" time t))

(defun trim-end (s)
  "Delete any whitespace at the end of a string."
  (replace-regexp-in-string "\\( \\|\f\\|\t\\|\n\\)+$" "" s))

(defun trim-start (s)
  "Delete any whitespace at the start of a string."
  (replace-regexp-in-string "^\\( \\|\f\\|\t\\|\n\\)+" "" s))

(defun trim (s)
  "Delete any whitespace at the start or end of a string."
  (trim-start (trim-end s)))


(defun t:zap-to-char (arg char)
  "Kill up to (but not including ARGth) occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive "p\ncZap to char: ")
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
                         (search-forward (char-to-string char) nil nil arg)
                         (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
                         (point))))
(tkb-keys ((kbd "M-Z") #'t:zap-to-char))

(defun tkb-push-env-var (var newval &optional first)
  (let ((val (getenv var)))
    (setenv var (if val
                    (if first
                        (concat newval ":" val)
                      (concat val ":" newval))
                  newval))))


;;; http://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))



(progn
;;; http://www.danielehrman.com/blog/2014/5/25/11-must-haves-for-every-power-programmer
(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (name-last-kbd-macro name)            ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)
)

(defun tkb-list-properties ()
  "Show text properties at point."
  (interactive)
  (message "%s" (text-properties-at (point))))

(defun tkb-find-in-path (path to-find)
  (let ((path (tkb-path-get path)))
    (catch 'checking
      (while path
        (let* ((directory (car path))
               (filename (concat (file-name-as-directory directory) to-find)))
          (if (file-exists-p filename) (throw 'checking filename)))
        (pop path)))))
;;; end of tkb-functions.el
