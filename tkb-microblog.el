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

(defun tkb-get-iso8601-date-time ()
  (interactive)
  (let* ((default-date-string (format-time-string "%FT%H:%M:%S%z"))
         (default-date-string (concat (substring default-date-string 0 22)
                                      ":"
                                      (substring default-date-string 22 24)))
         (date (iso8601-parse
                (read-string "Date? " default-date-string))))
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

(defun tkb-microblog-sub-blogs ()
  (interactive)
  (let* ((microblog-directory (f-join tkb-microblog-repo "gmi" "blog"))
         (subblogs (directory-files microblog-directory nil "blog-.*.gmi")))
    subblogs))
  
(defun tkb-microblog (specify-date-p)
  "Create a Gemtext document for the current day in my microblog.  A prefix
argument of - just opens the blog entry for the currrent day without adding
to it.  Any other prefix argument prompts for the date to use for the blog
entry instead."
  (interactive "P")
  (message "specify-date-p: %S" specify-date-p)
  (when specify-date-p
    (unless (y-or-n-p "If you are TIME TRAVELLING you may have fix the blog index yourself! Ok?")
      (user-error "You refused to accept responsibility for TIME TRAVELLING, so we're quitting!")))
  (cl-labels ((get-category (sublog)
                (let ((index (string-match "^blog-\\(.+\\)\\.gmi$" sublog)))
                  (if index
                      (match-string 1 sublog)
                    nil)))
              (get-categories (sub-blogs)
                (loop for sub-blog in sub-blogs
                      collect (get-category sub-blog)))
              (get-index-filenames (indexes lookup)
                (mapcar (lambda (index)
                          (let ((filename (cdr (assoc index lookup))))
                            (if filename
                                filename
                              (concat "blog-" index ".gmi"))))
                        indexes)))
                                          
    (let* ((title-sep " - ")
           (date                      (if specify-date-p
                                          (encode-time
                                           (tkb-get-iso8601-date-time))
                                        (current-time)))   
           (microblog-title           (read-string "Microblog title? "))
           (date-string               (format-time-string "%F" date))
           (year-string               (format-time-string "%Y" date))
           (tz-offset                 (format-time-string "%z"))
           (tz-offset                 (concat (substring tz-offset 0 3)
                                              ":"
                                              (substring tz-offset 3 5)))
           (time-string               (concat (format-time-string "%H:%M:%S")
                                              tz-offset))
           (dirname                   year-string)
           (filename                  (concat date-string "-"
                                              (tkb-sanitize-for-filename
                                               microblog-title t)))
           (gemtext-filename          (concat filename ".gmi"))
           (html-filename             (concat filename ".html"))
           (relative-html-filename    (f-join dirname html-filename))
           (relative-gemtext-filename (f-join dirname gemtext-filename))
           (site-index-filename       (f-join tkb-microblog-repo "gmi" "index.gmi"))
           (microblog-directory       (f-join tkb-microblog-repo "gmi" "blog"))
           (microblog-entry-directory (f-join microblog-directory dirname))
           (gemtext-pathname          (f-join microblog-entry-directory
                                              gemtext-filename))
           (blog-index-pathname       (f-join microblog-directory "blog.gmi"))
           (already-exists-p          (f-exists-p gemtext-pathname))
           (sub-blogs                 (tkb-microblog-sub-blogs))
           (categories                (get-categories sub-blogs))
           (lookup                    (loop for sub-blog in sub-blogs for category in categories
                                            collect (cons category sub-blog)))
           (indexes                   (completing-read-multiple "Sub-Blogs? " categories))
           (sub-blog-filenames        (cons "blog.gmi"
                                            (get-index-filenames indexes lookup))))
      (unless (f-exists-p microblog-entry-directory)
        (make-directory microblog-entry-directory t))
      (cl-flet ((add-to-index (index-filename)
                  (let* ((index-file (f-join microblog-directory index-filename))
                         (buf (find-file-noselect index-file)))
                    (save-excursion
                      (with-current-buffer buf
                        (unless (f-exists-p index-file)
                          (let* ((category (capitalize
                                            (s-replace-regexp
                                             "[-_.]" " "  (progn (string-match "^blog-\\(.+\\)\\.gmi$"
                                                                               index-filename)
                                                                 (match-string 1 index-filename))))))
                            (insert "# " category "\n\n")
                            ;; ??? Need to insert a link to the new blog index into the main site index as well.
                            (let ((site-index-buf (find-file-noselect site-index-filename))
                                  (relative-index-filename (f-join "blog" index-filename)))
                              (with-current-buffer site-index-buf
                                (goto-char (point-max))
                                (unless (looking-at "^")
                                  (insert "\n"))
                                (insert "=> " relative-index-filename " T. Kurt Bond's µBlog, " category " Sub-Blog\n")
                                (save-buffer)))
                            ))
                        (goto-char (point-min))
                        (if (re-search-forward "^=>" nil t) ; If there are entries
                            (beginning-of-line) ; add the new one there.
                          (goto-char (point-max))) ; Otherwise, add it at the end of the buffer.
                        (insert "=> " relative-gemtext-filename " " date-string " "
                                time-string title-sep microblog-title "\n")
                        (save-buffer))))))
        (cl-loop for sub-blog-filename in sub-blog-filenames do (add-to-index sub-blog-filename)))
      (find-file gemtext-pathname)
      (insert "# " date-string " " time-string title-sep microblog-title "\n\n"))))
;;; end of tkb-microblog.el
