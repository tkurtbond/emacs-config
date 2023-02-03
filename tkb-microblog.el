;;;; tkb-microblog.el - Stuff to help edit my microblog.

(defun tkb-find-file-hook ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.gmi\\'" buffer-file-name))
    (visual-line-mode 1)
    (flyspell-mode 1)
    (auto-fill-mode -1)))

(add-hook 'find-file-hook #'tkb-find-file-hook)

(defvar tkb-microblog-repo "~/Repos/microblog"
  "Location of the git repository for my microblog.")

(unless (fboundp 'iso8601-parse-date)                         ; Cry and wail!!!
  (load-library "tkb-iso8601"))

(defun tkb-get-iso8601-date ()
  (interactive)
  (let* ((default-date-string (format-time-string "%F"))
         (date (iso8601-parse-date
                (read-string "Date? " default-date-string))))
    (setf (car date) 0)
    (setf (cadr date) 0)
    (setf (caddr date) 0)
    date))

(defun tkb-dont-do-that ()
  (interactive)
  (message "Don't DO that!  It hurts!")
  (beep))

(defvar tkb-gmi-minor-mode-map (make-sparse-keymap)
  "Keymap while tkb-gmi-minor-mode is active.")

(define-minor-mode tkb-gmi-minor-mode
  "Minor mode to simulate buffer local keybindings."
  :init-value nil
  :lighter " GMI"
  tkb-gmi-minor-mode-map)
(define-key tkb-gmi-minor-mode-map (kbd "M-q") 'tkb-dont-do-that)

(add-to-list 'auto-mode-alist '("\\.gmi\\'" . tkb-gmi-minor-mode))

(defun tkb-microblog (specify-date-p)
  "Create a Gemtext document for the current day in my microblog.  A prefix
argument of - just opens the blog entry for the currrent day without adding
to it.  Any other prefix argument prompts for the date to use for the blog
entry instead."
  (interactive "P")
  (message "specify-date-p: %S" specify-date-p)
  (when (and specify-date-p (not (eq specify-date-p '-)))
    (message "If you are TIME TRAVELLING you may have fix the blog index yourself!")
    (beep))
  (let* ((title-sep " - ")
         (date (if (and specify-date-p (not (eq specify-date-p '-)))
                   (encode-time (tkb-get-iso8601-date))
                 (current-time)))
         (date-string (format-time-string "%F" date))
         (year-string (format-time-string "%Y" date))
         (time-string (format-time-string "%H:%M %Z"))
         (dirname year-string)
         (filename date-string)
         ;; I thought about doing this:
         ;;(filename (concat date "-"
         ;;                  (tkb-sanitize-for-filename microblog-title)))
         ;; but decided I only wanted one blog file for a day, so the title
         ;; shouldn't be in the name.
         (gemtext-filename    (concat filename ".gmi"))
         (html-filename       (concat filename ".html"))
         (relative-html-filename (f-join dirname html-filename))
         (microblog-directory (f-join tkb-microblog-repo "gmi" "blog"))
         (microblog-entry-directory (f-join microblog-directory dirname))
         (gemtext-pathname    (f-join microblog-entry-directory
                                      gemtext-filename))
         (blog-index-pathname (f-join microblog-directory "blog.gmi"))
         (already-exists-p    (f-exists-p gemtext-pathname)))
    (unless (f-exists-p microblog-entry-directory)
      (make-directory microblog-entry-directory t))
    (find-file gemtext-pathname)

    (unless (and specify-date-p (eq specify-date-p '-))
      (let ((microblog-title (read-string "Microblog title? ")))
        (cond
         (already-exists-p
          (goto-char (point-max))
          (beginning-of-line)
          (unless (looking-at "^[ \t]*$")
            (end-of-line)
            (insert "\n"))
          (insert "\n## " time-string title-sep microblog-title "\n\n"))
         (t
          (let ((buf (find-file-noselect blog-index-pathname)))
            (save-excursion
              (with-current-buffer buf
                ;; Save it in the blog index, blog.gmi
                (if (re-search-forward "^=>" nil t)
                    (beginning-of-line))
                (insert "=> " relative-html-filename " " date-string " "
                        time-string title-sep microblog-title "\n")
                (save-buffer)))
            (insert "# " date-string " " time-string title-sep
                    microblog-title))))))))
;;; end of tkb-microblog.el
