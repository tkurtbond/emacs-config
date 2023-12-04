;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-experimental.el -- Experimental -*- coding-system: utf-8 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #$ Outputs the current filename.  It's not supposed to be used in lisp code.
(message "%s" #$)

;;; Highlight cursor line: too annoying.
;;(global-hl-line-mode +1)
;;(global-hl-line-mode -1)

(eval-after-load "w3m"
  '(define-key w3m-mode-map "f" #'w3m-find-file))

(defun tkb-unhex-region ()
  (interactive)
  (let* ((s (buffer-substring (point) (mark)))
         (s* (url-unhex-string s t)))
    (kill-region (point) (mark))
    (insert s*)))

(defun tkb-unhex-current-kill ()
  "Unhex url from current kill."
  (interactive)
  (let* ((s (current-kill 0))
         (s (url-unhex-string s t)))
    (kill-new s)))
(tkb-keys ((kbd "C-c k U h") #'tkb-unhex-current-kill))

(defun tkb-rst-print-subheads ()
  "Print the recommend underlines with levels for the underline characters
recommended by the ReST quickref: http://tinyurl.com/47lkhk"
  (interactive)
  (with-work-buffer " *ReST heads"
      (cl-loop for c
            ;; recommended list from the rst quickref: http://tinyurl.com/47lkhk
            across "=-`:'\"~^_*+#<>"
            for i from 1
            do (let* ((s (format "Depth %d" i))
                      (u (make-string (length s) c)))
                 (insert (format "%s\n%s\n\n" s u))))))

(defun tkb-select-frame ()
  (interactive)
  (let* ((frames (cl-loop for frame in (frame-list)
                       collect (cons (frame-parameter frame 'name)
                                     frame)
                       into frames finally return frames))
         (frame-name (completing-read "Frame? " frames))
         (frame (cdr (assoc-string frame-name frames))))
    (raise-frame frame)
    (select-frame frame)))

(defun tkb-select-frame-popup ()
  (interactive)
  (let* ((frames (cl-loop for frame in (frame-list)
                          collect (cons (frame-parameter frame 'name)
                                        frame)
                          into frames finally return frames))
         (frame (x-popup-menu t `("Pick a frame" ("frames" ,@frames)))))
    (when frame
      (raise-frame frame)
      (select-frame-set-input-focus frame)
      )))
(tkb-keys ((kbd "C-c A") 'tkb-select-frame-popup))

(defun tkb-show-frame-size-and-position ()
  (interactive)
  (destructuring-bind (x-offset . y-offset) (frame-position)
    (let ((height (frame-height))
          (width  (frame-width)))
      (message "%dx%d at %d,%d" width height x-offset y-offset))))

(defun tkb-load-file ()
  "Load the current file into emacs lisp"
  (interactive)
  (load-file (buffer-file-name)))
(tkb-keys :keymap emacs-lisp-mode-map
          ((kbd "C-x L") #'tkb-load-file))

(defun tkb-locate-file (filename)
  (interactive "M")
  (message "file: %s" (locate-file filename load-path (get-load-suffixes))))

(when t					; Using org-capture now.
  (progn                                ; Config for mobile org
    ;; Set to the location of your Org files on your local system
    (setq org-directory "~/Org")
    (setq org-adapt-indentation t)
    (eval-after-load "org-mobile"
      '(progn 
         ;; Set to the name of the file where new notes will be stored
         (setq org-mobile-inbox-for-pull "~/Org/flagged.org")
         ;; Set to <your Dropbox root directory>/MobileOrg.
         (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
         (push "~/Org" org-mobile-files))))
  ;;
  (defconst tkb-org-journal      (expand-file-name "~/Repos/tkb-notes/Org/journal.org"))
  (defconst tkb-org-contacts     (expand-file-name "~/Repos/tkb-notes/Org/contacts.org"))
  (defconst tkb-org-books-read   (expand-file-name "~/Repos/tkb-notes/Books/read.org"))
  (defconst tkb-org-health       (expand-file-name "~/Repos/tkb-notes/Health/interactions.org"))
  (defconst tkb-org-notes        (expand-file-name "~/Repos/tkb-notes/Org/notes.org"))
  (defconst tkb-org-rpg          (expand-file-name "~/Repos/tkb-notes/Org/rpg.org"))
  (defconst tkb-org-tasks        (expand-file-name "~/Repos/tkb-notes/Org/tasks.org"))
  (defconst tkb-org-video        (expand-file-name "~/Repos/tkb-notes/Org/video.org"))
  (defconst tkb-org-blog-ideas   (expand-file-name "~/Repos/tkb-notes/Org/blog-ideas.org"))
  (defconst tkb-org-gaming-ideas (expand-file-name "~/Repos/tkb-notes/RPG/gaming-ideas.org"))
  (defconst tkb-org-mpl-journal  (expand-file-name "~/Repos/tkb-notes/MPL/Org/journal.org"))
  (defconst tkb-org-mhst-journal (expand-file-name "~/job/MPL/MHST/Org/mhst-journal.org"))
  (defconst tkb-org-mpl-contacts (expand-file-name "~/Repos/tkb-notes/MPL/Org/contacts.org"))
  (defconst tkb-org-mpl-notes    (expand-file-name "~/Repos/tkb-notes/MPL/Org/notes.org"))
  (defconst tkb-org-mpl-tasks    (expand-file-name "~/Repos/tkb-notes/MPL/Org/tasks.org"))
  (tkb-keys ((kbd "C-c k o c") #'org-capture))

  (defvar tkb-org-year (format-time-string "%Y")
    "The year the current emacs session was started, for use with org-capture.")
  (defun tkb-org-capture-advice-update-year (&optional goto keys)
    "Update ‘tkb-org-year’ and update the entry for adding a book in 
‘org-capture-templates’ to use the new value."
    (let ((new-year (format-time-string "%Y")))
      (unless (string-equal tkb-org-year new-year)
        (setf tkb-org-year new-year)
        (setf (--> "b" (assoc it org-capture-templates)
                   (assoc 'file+olp it) (nth 2 it))
              tkb-org-year))))
  (advice-add 'org-capture :before #'tkb-org-capture-advice-update-year)
  
  (setq org-capture-templates
        `(("X" "EXPERIMENT" entry
           (file+olp+datetree ,(expand-file-name "~/current/org/loud-experiment.org"))
           "*** %^{Title} %U\n  %i\n  %?\n")
          ("b" "Add book about to read" entry
           (file+olp ,(expand-file-name tkb-org-books-read)
                     ,(format-time-string "%Y") "Read")
           "*** : %c" :prepend t)
          ("j" "Journal" entry
           (file+headline ,tkb-org-journal "Journal")
           "* %^{Title} %U\n  %i\n  %?\n")
          ("c" "Contacts Log" entry
           (file+headline ,tkb-org-contacts "Contacts")
           "* %^{Title} %U\n  %i%?\n")
          ("B" "Blog Ideas" entry
               (file ,tkb-org-blog-ideas)
               "* %^{Title} %U\n  %i%?\n")
          ("g" "Gaming Ideas" entry
               (file ,tkb-org-gaming-ideas)
               "* %^{Title} %U\n  %i%?\n")
          ("h" "Health" entry
           (file ,tkb-org-health)
           "* %^{Title} %U\n  %i%?\n")
          ("n" "Notes" entry
           (file+headline ,tkb-org-notes "Notes")
           "\n\n* %^{Title} %U\n  %i\n  %?\n  %a\n\n")
          ("r" "RPG" entry
           (file+headline ,tkb-org-rpg "RPG")
           "\n\n* %^{Title} %U\n  %i\n  %?\n  %a\n\n")
          ("t" "Tasks" entry
           (file+headline ,tkb-org-tasks "Tasks")
           "* TODO %^{Title} %U\n  %i\n  %?\n  %a\n")
          ("v" "Video" entry
           (file+headline ,tkb-org-video "Video")
           "* TODO %^{Title} %U\n  %^C%i%?\n")
          ("J" "MPL Journal" entry
           (file+headline ,tkb-org-mpl-journal "MPL Journal")
           "* %^{Title} %U\n  %i\n  %?\n")
          ("M" "MHST Journal" entry
           (file+headline ,tkb-org-mhst-journal "MHST Journal")
           "* %^{Title} %U\n  %i\n  %?\n")
          ("C" "MPL Contacts Log" entry
           (file+headline ,tkb-org-mpl-contacts "MPL Contacts")
           "* %^{Title} %U\n  %i\n  %?\n")
          ("N" "MPL Notes" entry
           (file+headline ,tkb-org-mpl-notes "MPL Notes")
           "\n\n* %^{Title} %U\n  %i\n  %?\n  %a\n\n")
          ("T" "MPL Tasks" entry
           (file+headline ,tkb-org-mpl-tasks "MPL Tasks")
           "* TODO %^{Title} %U\n  %i\n  %?\n  %a\n")))
  ;;(defvar tkb-org-files-map (make-sparse-keymap))
  (define-prefix-command 'tkb-org-files-map)
  (global-set-key (kbd "C-C k o F") 'tkb-org-files-map)
  (tkb-keys ((kbd "C-c k o C-c") #'org-ctrl-c-ctrl-c)
            ((kbd "C-c k o F B") #'(lambda () (interactive)
                                     (find-file tkb-org-blog-ideas)))
            ((kbd "C-c k o F j") #'(lambda () (interactive)
                                     (find-file tkb-org-journal)))
            ((kbd "C-c k o F c") #'(lambda () (interactive)
                                     (find-file tkb-org-contacts)))
            ((kbd "C-c k o F h") #'(lambda () (interactive)
                                     (find-file tkb-org-health)))
            ((kbd "C-c k o F n") #'(lambda () (interactive)
                                     (find-file tkb-org-notes)))
            ((kbd "C-c k o F r") #'(lambda () (interactive)
                                     (find-file tkb-org-rpg)))
            ((kbd "C-c k o F t") #'(lambda () (interactive)
                                     (find-file tkb-org-tasks)))
            ((kbd "C-c k o F v") #'(lambda () (interactive)
                                     (find-file tkb-org-video)))
            ((kbd "C-c k o F J") #'(lambda () (interactive)
                                     (find-file tkb-org-mpl-journal)))
            ((kbd "C-c k o F M") #'(lambda () (interactive)
                                     (find-file tkb-org-mhst-journal)))
            ((kbd "C-c k o F C") #'(lambda () (interactive)
                                     (find-file tkb-org-mpl-contacts)))
            ((kbd "C-c k o F N") #'(lambda () (interactive)
                                     (find-file tkb-org-mpl-notes)))
            ((kbd "C-c k o F T") #'(lambda () (interactive)
                                     (find-file tkb-org-mpl-tasks)))
            )
  (tkb-check-bindings (list (kbd "C-c k o C-c")))

  (defun tkb-find-org-log-file ()
    "Look in the current directory or its parents for a file named *-log.org
and return it."
    ;; So I can us M-x tkb-find-file-org-log and not bring up the debugger on the
    ;; call to error when testing.
    (interactive)
    (let ((default-directory default-directory)
          (original-directory default-directory)
          log-files)
      (while (not (setq log-files (file-expand-wildcards "*-log.org" t)))
        (cd "..")
        (when (string-equal default-directory "/")
          (error "unable to find *-log.org starting at %s"
                 original-directory)))
      (car log-files)))

  (defun tkb-find-file-org-log ()
    "Look in the current directory or its parents for a file named *-log.org
and switch to a buffer visiting it."
    (interactive)
    (let* ((org-file (tkb-find-org-log-file)))
      (find-file org-file)))
  (tkb-keys ((kbd "C-c k o f") #'tkb-find-file-org-log))
  ;; (defalias 'x #'tkb-find-file-org-log)

  (defun tkb-add-org-log ()
    "Look in the current directory or its parents for a file named *-log.org
and add a log entry to it."
    (interactive)
    (let* ((org-file (tkb-find-org-log-file))
           (org-capture-templates
            `(("l" "Log" entry (file+headline ,org-file "Log")
               "* %^{Title} %U\n  %i\n  %?\n"))))
      (org-capture)))
  (tkb-keys ((kbd "C-c k o l") #'tkb-add-org-log)))

(progn
  ;; See Info: (org)Activation.
  ;; The following lines are always needed.  Choose your own keys.
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (tkb-keys ((kbd "C-c k o s") #'org-store-link)
            ((kbd "C-c k o a") #'org-agenda))
  (add-hook 'org-mode-hook 'turn-on-font-lock))	; org-mode buffers only

(setq org-use-sub-superscripts '{}
      org-export-with-sub-superscripts '{})
(tkb-keys ((kbd "C-c k o TAB") #'org-global-cycle))

(defun tkb-toggle-trailing-whitespace-display ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))


(defun increment-filename (prefix &optional suffix always start)
  "Generate a unique filename using PREFIX and optionally SUFFIX"
  (let* ((suffix (if suffix suffix ""))
         (start (if start start 0))
         (sep1   "_")
         (sep2   "_")
         (fileprefix (concat prefix)))
    (cl-loop for i from start
          ;; The zeroth filename doesn't have the number.
          for testname = (if always
                             (format "%s%s%d%s" fileprefix sep2 i suffix)
                           (format "%s%s" fileprefix suffix))
          then (format "%s%s%d%s" fileprefix sep2 i suffix)
          until (not (file-exists-p testname))
          finally return testname)))

(defun increment-filename-date (prefix &optional suffix always date)
  "Generate a unique filename using PREFIX and optionally SUFFIX"
  (let* ((suffix (if suffix suffix ""))
         (sep1   "_")
         (sep2   "_")
         (date   (if date
                     (if (listp date)
                         (format-time-string "%F" date)
                       date)
                   (format-time-string "%F")))
         (fileprefix (concat prefix sep1 date)))
    (cl-loop for i from 0
          ;; The zeroth filename doesn't have the number.
          for testname = (if always
                             (format "%s%s%d%s" fileprefix sep2 i suffix)
                           (format "%s%s" fileprefix suffix))
          then (format "%s%s%d%s" fileprefix sep2 i suffix)
          until (not (file-exists-p testname))
          finally return testname)))

(defun increment-filename-date-prefix (prefix &optional suffix always date)
  "Generate a unique filename using PREFIX and optionally SUFFIX"
  (let* ((suffix (if suffix suffix ""))
         (sep1   "_")
         (sep2   "_")
         (date   (if date
                     (if (listp date)
                         (format-time-string "%F" date)
                       date)
                   (format-time-string "%F")))
         (dirname (file-name-directory prefix))
         (filename (file-name-nondirectory prefix))
         (fileprefix (concat dirname date sep1 filename)))
    (cl-loop for i from 0
          ;; The zeroth filename doesn't have the number, unless ALWAYS
          for testname = (if always
                             (format "%s%s%d%s" fileprefix sep2 i suffix)
                             (format "%s%s" fileprefix suffix))
          then (format "%s%s%d%s" fileprefix sep2 i suffix)
          until (not (file-exists-p testname))
          finally return testname)))


(defun rename-buffer-uniquely ()
  "Rename the current buffer to something that will be unique."
  ;; (rename-buffer x t) and (generate-new-buffer-name) only work if
  ;; buffers with the names to be avoided already exist.  I want
  ;; something so I can rename a buffer so I can have things like two
  ;; greps running at once.
  (interactive)
  (rename-buffer (cl-loop with bufname =
                       (progn
                         (if (string-match (rx (and
                                            string-start
                                            (group (+ anything))
                                            (group
                                             (? "[0-9]+"))
                                            string-end))
                                       (buffer-name))
                             (match-string 1)
                             (buffer-name)))
                       for i from 1
                       with newname = (format "%s-%d" bufname i)
                       while (get-buffer newname)
                       finally return newname)))


(setq longlines-show-hard-newlines t)
(progn
  ;; http://jfm3-repl.blogspot.com/2006/06/emacs-tricks-1-completion.html
  (require 'completion)
  (dynamic-completion-mode)
  (global-set-key (kbd "M-<return>") 'complete))

(unless (or window-system (member system-name '("consp.org")))
  (load "tkb-tty-colors.el"))


(setq tkb-color-list '(("black" "white")
                       ("white" "black")
                       ("green" "black")
                       ("black" "green")
                       ("black" "wheat")
                       ("black" "navajowhite1")))
(defun tkb-next-colors ()
  "Goto the next next set of background and foreground colors."
  (interactive)
  (let* ((fg (frame-parameter nil 'foreground-color))
         (bg (frame-parameter nil 'background-color))
         (colors (list fg bg))
         (x (cadr (member colors tkb-color-list))))
    (if x
        (cl-destructuring-bind (nfg nbg) x
          (set-frame-parameter nil 'foreground-color nfg)
          (set-frame-parameter nil 'background-color nbg)
          (message "(%s %s" nfg nbg))
      (cl-destructuring-bind (nfg nbg) (car tkb-color-list)
         (set-frame-parameter nil 'foreground-color nfg)
         (set-frame-parameter nil 'background-color nbg)
         (message "(%s %s" nfg nbg)))))

(defun tkb-wheat ()
  (interactive)
  (set-frame-parameter nil 'foreground-color "black")
  (set-frame-parameter nil 'background-color "wheat"))

(defun nothing ()
  ;; Why did I need this???
  (interactive)
  nil)

(unless window-system
  (xterm-mouse-mode 1))

(when nil
  ;; Apparently eval doesn't macroexpand the function part of (function ...)
  (defmacro lambda* (&rest rest)
    (let ((buf (get-buffer-create "*lambda**")))
      (pp rest buf)
      (let ((e (cl-transform-lambda rest nil)))
        (pp e buf)
        (let ((l `(lambda ,@(cdr e))))
          (pp l buf)
          (eval l)))))
  (funcall (lambda* (&optional &key a &key (b 2)) (list a b)) :a 1)
  ((lambda* (&optional &key a &key (b 2)) (list a b))))

(defun tkb-python-indent-statement ()
  (interactive)
  (let (b e)
  (save-excursion
    (python-beginning-of-statement)
    (setq b (point))
    (python-end-of-statement)
    (setq e (point))
    (python-indent-region b e))))


(eval-after-load "python"
  '
  (progn
    (tkb-keys :keymap python-mode-map
              ([(control meta ?q)] #'tkb-python-indent-statement))
    ))

;; This isn't working
(defadvice python-load-file (before tkb-py-load-file (file-name) activate)
  "Expand FILE-NAME using `expand-file-name'."
  (setq file-name (expand-file-name file-name)))


;; Should be part of MSWoe???
(setq potential-pythons
      ["c:/Python26/python.exe" "c:/Python25/python.exe" "python"])
(when-file (p potential-pythons)
  (setq python-command p
        python-python-command p))
(defun tkb-toggle-pythons ()
  (interactive)
  (let ((old python-command))
    (if (not (string-equal python-command "python"))
        (setq python-command "python")
      (setq python-command
            (find-if #'file-executable-p
                     potential-pythons)))
    (message "Switching pythons from %s to %s" old python-command)))


(eval-after-load "rst.el"
  '(begin
    (modify-syntax-entry ?“ "(" rst-mode-syntax-table)
    (modify-syntax-entry ?” ")" rst-mode-syntax-table)
    (modify-syntax-entry ?‘ "(" rst-mode-syntax-table)
    (modify-syntax-entry ?’ ")" rst-mode-syntax-table)
    (custom-set-faces
    '(rst-level-1-face ((t (:background "red"))) t)
    '(rst-level-2-face ((t (:background "blue"))) t)
    '(rst-level-3-face ((t (:background "yellow"))) t)
    '(rst-level-4-face ((t (:background "magenta"))) t)
    '(rst-level-5-face ((t (:background "white"))) t))))


(defun tkb-continue-line (n)
  (interactive "P")
  (if n
      (setq n (prefix-numeric-value n))
    (setq n 79))
  (let ((start-char (save-excursion (beginning-of-line)
                                    (buffer-substring (point) (1+ (point))))))
    (save-excursion
      (end-of-line)
      (while (< (current-column) 80)
        (insert start-char)))))
(tkb-keys ((kbd "C-c k c") #'tkb-continue-line))

(when-directory (d (expand-file-name "~/lib/data/fortunes"))
  (setq fortune-dir d))
(setq fortune-file "~/lib/data/fortunes")

(tkb-keys ((kbd "C-c k h") #'hyperspec-lookup))


(autoload 'fortune-in-buffer "fortune")
(defun tkb-fortune-string (&optional file)
  (interactive
   (list
    (if current-prefix-arg
        (fortune-ask-file)
      fortune-dir)))                    ;was fortune-file
  (save-excursion
    (fortune-in-buffer t file)
    (set-buffer fortune-buffer-name)
    (let* ((fortune (buffer-string)))
      ;;(substitute ?\ ?\n fortune)
      fortune)))


(defconst tkb-random-me
  ["Crazy K Ranch"
   "Sorry to say"
   "Simple is as simple does"
   "WTF!"
   "I have SEEN the CONSING!!"
   "Yow!"
   "Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!"
   ])
(defun tkb-random-me ()
  (interactive)
  (aref tkb-random-me (random (length tkb-random-me))))


(modify-coding-system-alist 'file "\\.rst\\'" 'utf-8-unix)
(modify-coding-system-alist 'file "\\.org\\'" 'utf-8-unix)
;; This is for unison.  Unfortunately, you can't include the .unison.
(modify-coding-system-alist 'file "\\.prf\\'" 'utf-8-unix)

;; http://www.neverfriday.com/sweetfriday/2008/06/emacs-tip-word-counting-with-a.html
(defun wc ()
  (interactive)
  (message "Word count: %s" (how-many "\\w+" (point-min) (point-max))))

(defun tkb-goto-info-node (arg)
  "Goto an Info node in normal text, specified as \"Info: (FILE)Node.\"
where the \"FILE\" is optional and the \".\" can also be a \",\"."
  (interactive "P")
  (require 'info)
  (save-excursion
    (when (re-search-forward "Info: \\(\\((.*)\\)*[^.,]*\\)" nil t)
      (let ((s (match-string 1)))
        (when (y-or-n-p (concat "Goto Info node " s " ? "))
          (Info-goto-node s arg))))))
(tkb-keys ((kbd "C-c k i") #'tkb-goto-info-node))
;; Info: (info)Go to node



;; http://article.gmane.org/gmane.emacs.macintosh.osx/107
(standard-display-8bit 128 255)


;; http://nex-3.com/posts/45-efficient-window-switching-in-emacs
(tkb-keys :just-warn
  ([M-left]  'windmove-left)		; move to left windnow
  ([M-right] 'windmove-right)		; move to right window
  ([M-up]    'windmove-up)		; move to upper window
  ([M-down]  'windmove-down)		; move to downer window
  )


(defun tkb-move-frame-left ()
  "Move the current frame left by 1/10th the width of the physical montor."
  (interactive)
  (let* ((left (frame-parameter nil 'left))
         (monitor-display-width (caddr (frame-monitor-attribute 'geometry)))
         (tenth-width (/ monitor-display-width 10))
         (new-left (- left tenth-width)))
    (set-frame-parameter nil 'left new-left)))
(tkb-keys ((kbd "C-c k F l") #'tkb-move-frame-left))

(defun tkb-move-frame-right ()
  "Move the current frame right by 1/10th the width of the physical montor."
  (interactive)
  (let* ((left (frame-parameter nil 'left))
         (monitor-display-width (caddr (frame-monitor-attribute 'geometry)))
         (tenth-width (/ monitor-display-width 10))
         (new-left (+ left tenth-width)))
    (set-frame-parameter nil 'left new-left)))
(tkb-keys ((kbd "C-c k F r") #'tkb-move-frame-right))

(defadvice browse-url (before tkb-advise-browse-url activate)
  (let ((all-args (ad-get-args 0))
        (rest-args (ad-get-args 1)))
    (when (and (not rest-args)
               (boundp 'fork)
               (symbol-value 'fork))
      (ad-set-args 1 (symbol-value 'fork)))
    (message "tkb-advise-browse-url: bound: %S args: %S" (boundp 'fork) args)))

(defun tkb-browse-url-eww (url &optional args)
  "Invoke the eww browser (inside emacs) on URL.  It the optional second
argument ARGS is true open in a new buffer."
  (message "args: %S" args)
  (eww url (and args 4)))

(defun tkb-toggle-eww ()
  (interactive)
  (cond ((eq browse-url-browser-function 'browse-url-default-browser)
         (message "Switching to EWW for opening URLs.")
         (setq browse-url-browser-function #'tkb-browse-url-eww))
        (t
         (message "Switching to browser-url-default-browser for opening URLs.")
         (setq browse-url-browser-function #'browse-url-default-browser))))

(defun tkb-cleanup-title (title)
  (let ((s
         (substitute ?- 32
                     (remove-if-not #'(lambda (c) (or (memq c '(?- 32 ?.))
                                                      (and (<= ?a c)
                                                           (<= c ?z))
                                                      (and (<= ?0 c)
                                                           (<= c ?9))))
                                    (downcase title)))))
    (while (string-match "\\([^[:alnum:]]\\)\\1+" s)
      (let ((c (match-string 1 s))
            (b (match-beginning 0))
            (e (match-end 0)))
        (setq s (concat (substring s 0 (1+ b)) (substring s e)))))
    (while (string-match "\\.-" s)
      (setq s (concat (substring s 0 (match-beginning 0))
                      "-"
                      (substring s (match-end 0)))))
    s))

(defun tkb-insert-rst-section (title char &optional above)
  (let* ((s (tkb-rst-section-underline title char)))
    (when above (insert s "\n"))
    (insert title "\n" s "\n")))

(defun tkb-rst-section-underline (title char)
  (make-string (length title) char))


;; http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
;; describe-char-unicodedata-file
(defun tkb-load-unicode-txt ()
  (let* ((udf-url "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt")
         (udf-dest "~/tmp/UnicodeData.txt")
         (udf-dir  (file-name-directory udf-dest)))
    (if (file-readable-p udf-dest)
        (setq describe-char-unicodedata-file udf-dest)
      (when (y-or-n-p (format "You need to download %s ! Do it? " udf-url))
        ;; Really weird: wget -O 'file' complains that file doesn't exist!
        (unless (file-directory-p udf-dir)
          (make-directory udf-dir t))
        (let ((cmd (format "cd ~/tmp/ && wget -O %s --progress=dot '%s' &"
                           udf-dest udf-url)))
          (message "cmd: %s" cmd)
          (shell-command cmd)
          (setq describe-char-unicodedata-file "~/tmp/UnicodeData.txt"))))))

(eval-after-load "tkb-last" #'tkb-load-unicode-txt)


(defun fmt-duration (time)
  (cl-flet ((f (duration unit)
               (when duration (format (if (floatp duration)
                                          "%f%s"
                                        "%d%s")
                                      duration unit))))
    (cl-destructuring-bind (hi lo ms) time
      (let ((s (+ hi lo))
            (x "")
            (d nil)
            (h nil)
            (m nil))
        (when (>= s 86400)
          (setq d (/ s 86400)
                s (% s 86400)))
        (when (>= s 3600)
          (setq h (/ s 3600)
                s (% s 3600)))
        (when (>= s 60)
          (setq m (/ s 60)
                s (% s 60)))
        (unless (zerop ms)
          (setq s (+ s (/ ms 1000000.0))))
        (mapconcat 'identity
                   (remove-if #'null
                              (list (f d "d") (f h "h") (f m "m") (f s "s")))
                   " ")))))


(defmacro time (&rest body)
  `(let ((start (current-time)))
     (unwind-protect
         (progn ,@body)
       (let* ((end (current-time))
              (delta (time-subtract end start))
              (timebuf (get-buffer-create " *timing*")))
         (display-buffer timebuf)
         (princ (format "%s\n" (fmt-duration delta)) timebuf)))))

(defun tkb-blog (title tags category date &optional title-prefix no-increment)
  ;; This is obsolete.
  "Create a blog entry, prompting for various values and creating the
appropriate directory structure."
  ;;(interactive "sTitle: \nsTags: \nDCategory: \nsDate: ")
  (interactive (list
                (read-string "Title: ")
                (read-string "Tags: ")
                (read-directory-name "Category: " "~/myblog/entries/")
                (read-string "Date: ")))
  (let* ((date-time (if (listp date)
                        date
                      (if (empty-string-p date)
                          nil
                        (tkb-parse-iso-date date))))
         (no-spaces (tkb-cleanup-title title))
         (filename (funcall
                    (if no-increment
                        #'(lambda (&rest args)
                            (apply #'concatenate `(string ,@args)))
                      #'increment-filename)
                    (concat (file-name-as-directory
                             (expand-file-name category "~/myblog/entries/"))
                            no-spaces)
                    ".rst" nil))
         (dirname (file-name-directory filename))
         (published (format-time-string "#published %Y-%m-%d %H:%M:%S"
                                        date-time)))
    (if (not (file-directory-p dirname))
        (make-directory dirname 'and-parents))
    ;; FIXME: warn about existing files, ask to increment???
    (find-file filename)
    (unless (file-exists-p filename)
      (if title-prefix
          (insert title-prefix))
      (insert title)
      (insert "\n")
      (insert published)
      (insert "\n")
      (unless (zerop (length tags))
        (insert (concat "#tags " tags))
        (insert "\n")))))

(defun tkb-reading-monthly (date)
  (interactive "sDate: ")
  (let* ((date-time (if (empty-string-p date)
                        (current-time)
                      (tkb-parse-iso-date date)))
         (tags "recent reading")
         (category (format-time-string "books/read/%Y/%m" date-time)))
    (tkb-blog (format-time-string "Recent Reading: %B %Y" date-time)
              tags category date-time nil t)))

(defun tkb-reading (authors tags date)
  "Create a blog entry about my recent reading"
  (interactive "sAuthors: \nsTags: \nsDate: ")
  (let* ((date-time (if (empty-string-p date)
                        (current-time)
                      (tkb-parse-iso-date date)))
         (tags (concat "recent reading" (if (empty-string-p tags)
                                            ""
                                          (concat "," tags))))
         (category (format-time-string "books/read/%Y/%m" date-time)))
    (tkb-blog authors tags category date-time "Recent Reading: ")))

(defun tkb-viewing (titles tags date)
  "Create a blog entry about my recent viewing"
  (interactive "sTitles: \nsTags: \nsDate: ")
  (let* ((date-time (if (empty-string-p date)
                        (current-time)
                      (tkb-parse-iso-date date)))
         (tags (concat "recent viewing" (if (empty-string-p tags)
                                            ""
                                          (concat "," tags))))
         (category (format-time-string "media/viewing/%Y/%m" date-time)))
    (tkb-blog titles tags category date-time "Recent Viewing: ")))

(when-directory (d (expand-file-name "~/lib/emacs/others/misc"))
  ;; used by my hooks for rst and formerly asciidoc
  (add-to-list 'load-path d)
  ;; look at https://github.com/ndw/xmlunicode for xmlunicode.el
  ;; xmlunicode-character-list.el.  xmlunicode.el provides the
  ;; "smart-unicode-*" functions.
  (load-library "unichars")
  (load-library "xmlunicode")

  (defun tkb-describe-character (before)
    "Describe the character after point (before if a prefix was specified)
if it is a Unicode character."
    (interactive "P")
    (let* ((char (if before (char-before) (char-after)))
           (info (assoc (encode-char char 'ucs) unicode-character-list))
           (info (if info (cons (format "#x%X" (car info)) info)
                   "Not in this list, try 'C-c k D' (describe-char)")))
      (message "%S" info)))
  (tkb-keys ((kbd "C-c k d") #'tkb-describe-character))

  (defun tkb-insert-unicode-description (before)
    "Insert a description of the character after point (before if a prefix was
specified) if it is a Unicode character."
    (interactive "P")
    (let* ((char (if before (char-before) (char-after)))
           (name (get-char-code-property char 'name))
           (old-name (get-char-code-property char 'old-name))
           (desc (format "U+%04X %c %s%s" char char name
                         (if old-name (concat " (" old-name ")") ""))))
      (if before (delete-char -1) (delete-char 1))
      (insert desc)))
  (tkb-keys ((kbd "C-c k U") #'tkb-insert-unicode-description))

  (define-minor-mode tkb-smart-unicode-mode
    "Toggle smart unicode punctuation" nil " ♻⚔☣☥☸◉⅙✽☮" ; "✘▧▧⚅☑☢☹☺♠♥♦♣♨"
    :keymap '(("\"" . unicode-smart-double-quote)
              ("'"  . unicode-smart-single-quote)
              ("-"  . unicode-smart-hyphen)
              ("."  . unicode-smart-period)))

  (defadvice unicode-smart-hyphen (after tkb-after-unicode-smart-hyphen last
                                         activate compile)
    (tkb-describe-character t))


  )


(when nil				;retiring doc-mode 2019-11-03
  (autoload 'doc-mode "doc-mode")
  (add-to-list 'auto-mode-alist '("\\.adc$" . doc-mode))
  (eval-after-load "compile"
    '(add-to-list 'compilation-error-regexp-alist
                  '("^ERROR:[ \t]*\\([[:alpha:]][-[:alnum:].]+\\):[ \t]*line[ \t]*\\([0-9]+\\):" 1 2)))
  (eval-after-load "doc-mode" '
    (progn

      (defun tkb-asciidoc-version-increment ()
        (interactive)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^v\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\),[ \t]+\\(.+\\)"
                                   nil t)
            (let* ((part3 (match-string 3))
                   (date  (match-string 4))
                   (n     (string-to-number part3))
                   (new3  (format "%d" (1+ n))))
              (when (and (not (string-equal date (tkb-timestamp)))
                         (y-or-n-p (format "Sure replace %s with %s? " part3 new3)))
                (replace-match new3 nil t nil 3)
                (replace-match (tkb-timestamp) nil t nil 4))
              nil))))

      (defun bind-doc-mode-keys ()
        (set-language-environment "utf-8")
        (define-key doc-mode-map "\"" 'unicode-smart-double-quote)
        (define-key doc-mode-map "'" 'unicode-smart-single-quote)
        (define-key doc-mode-map "-" 'unicode-smart-hyphen)
        (define-key doc-mode-map "." 'unicode-smart-period)
        ;; display UniChar menu when in doc mode
        (define-key doc-mode-map [menu-bar unichar]
          (cons "UniChar" unicode-character-menu-map))
        ;; set input method to "xml" (xmlunicode) when in doc mode
        (set-input-method 'xml))

      (defun tkb-doc-mode-hook ()
        (add-hook 'write-contents-functions #'tkb-asciidoc-version-increment))

      (add-hook 'doc-mode-hook #'bind-doc-mode-keys)
      (add-hook 'doc-mode-hook #'tkb-doc-mode-hook)

      )))

(progn
  ;; Yank from emacs in screen
  (defun tkb-yank-from-screened-emacs ()
    (interactive)
    (let ((text (current-kill 0)))
      (when (string-match "^\\(.*\\)\\\\\n[ \t]*\\(.*\\)$" text)
        (insert (match-string 1 text))
        (insert (match-string 2 text)))))
  ;; This doesn't handle multiple lines properly
  (defun tkb-cleanup-yank-from-screened-emacs ()
    (interactive)
    (let ((text (current-kill 0)))
      (while (string-match "\\`\\(.*\\)\\\\\n\\([[:ascii:][:nonascii:]]*\\)\\'" text)
        (setq text (concat (match-string 1 text) (match-string 2 text))))
      (kill-new text))))

(defun t:kill-host-from-url ()
  (interactive)
  (require 'url-parse)
  (let* ((url (current-kill 0))
         (host (url-host (url-generic-parse-url url))))
    (kill-new host)
    (message "Killed %s\nto %s" url host)))

(tkb-keys ((kbd "C-c k u h") #'t:kill-host-from-url))

(when nil
  ;; 2023-06-27: I no longer remember what I was trying to do with this
  ;; function, so I'm going to steal it's keybinding for something else.
  (defun tkb-fill-list ()
    (interactive)
    (save-excursion
      (let ((paragraph-start "\f\\|[	]*$\\|\\(?:[*\f]+\\)")
            (paragraph-separate "[	\f]*$\\|\\(?:[*\f]+\\)"))
        (message "Buffer local variables: \n****\n%S\n****\n" (buffer-local-variables))
        (sit-for 1)
        (message "start: %S sep: %S" paragraph-start paragraph-separate)
        (sit-for 1)
        (mark-paragraph)
        (narrow-to-region (point) (mark))
        (sit-for 1)
        (fill-paragraph nil))))
  (tkb-keys ((kbd "C-c k f") 'tkb-fill-list)))

;; Gimme some register compatibility binding love!
(tkb-keys
  ((kbd "C-x /") 'point-to-register)		; C-x / became C-x r SPC
  ((kbd "C-x j") 'jump-to-register)		; C-x j became C-x r j
  ((kbd "C-x x") 'copy-to-register)		; C-x x became C-x r s
  ((kbd "C-x g") 'insert-register)		; C-x g became C-x r i
  )

(setq list-faces-sample-text
      "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 {}[]<>()\/ ~`!@#$%^&*_-+=|;:'?")
(progn
  (defun tkb-shell-command-on-this-file ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (unless file-name
        (error "Buffer %s is not visiting a file" (buffer-name)))

      (let ((args (list (read-from-minibuffer "Shell command: "
                                              (cons (concat " " file-name)
                                                    1)
                                              nil
                                              nil
                                              'shell-command-history)
                        current-prefix-arg
                        shell-command-default-error-buffer)))
        (apply 'shell-command args))))
  (tkb-keys ((kbd "C-c k !") 'tkb-shell-command-on-this-file)))


(fset 'tkb-nxml-end-of-element "\C-[\C-u\C-[\C-f")

(when-load-file "rng-auto" :load
                (setq auto-mode-alist
                      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
                            auto-mode-alist)))

(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map (kbd "C-c C-j") 'tkb-nxml-end-of-element)
     (setq nxml-sexp-element-flag t)))

(setq default-indicate-buffer-boundaries 'left) ;??? Too distracting???
(setq default-indicate-empty-lines t)

(defun bmi (height weight)
  "Calculate BMI, body mass index.  BMI <18.5 may be underweight, BMI of 18.5
to 25 may be optimal weight, BMI >25 may be overweight, BMI >30 is obese,
over 40 is morbidly obese, over 50 is super morbidly obese."
  (interactive "nHeight in inches: \nnWeight in pounds: ")
  (let* ((bmi (* 703 (/ (float weight) (expt height 2))))
         (characterization
          (cond ((<  bmi 18.5) "underweight")
                ((<= bmi 25.0) "optimal")
                ((<  bmi 30.0) "overweight")
                ((<  bmi 40.0) "obese")
                ((<  bmi 50.0) "morbidly obese")
                (t "super morbidly obese"))))
    (message "bmi: %06.3f; %s" bmi characterization)))


(when (not window-system)
  (eval-after-load "font-lock"
    '(progn
       (mapc #'(lambda (face)
                 (when (facep face)
                   (set-face-foreground face "cyan")))
             '(font-lock-comment-face sgml-doctype-face))
       ;;(set-face-foreground 'font-lock-comment-face "cyan")
       ;;'sgml-doctype-face "cyan")
       (message "!!!cyan rules!!!")))
  (eval-after-load "magit-diff"
    '(progn
       (set-face-foreground 'magit-diff-added-highlight "purple")
       (set-face-foreground 'magit-diff-added "blue")
       )))

(when nil
  (progn
    (setq version-control nil)
    ;; I think this keeps all your emacs backup files under one directory.
    (or (file-directory-p "~/backup") (make-directory "~/backup" t))
    (let ((backup-dir (expand-file-name "~/backup")))
      (setq backup-directory-alist
            `((".*" . ,backup-dir)))
      (setq auto-save-file-name-transforms
            `((".*" ,backup-dir t))))))
(progn
  (setq backup-directory-alist '(("." . ".~"))))

(defun tkb-next-blank-line ()
  (interactive)
  (while (not (looking-at "^[ \t]*$"))
    (forward-line)))
(tkb-keys ((kbd "C-c C-n") 'tkb-next-blank-line))

(defun tkb-previous-blank-line ()
  (interactive)
  (while (not (looking-at "^[ \t]*$"))
    (forward-line -1)))
(tkb-keys ((kbd "C-c C-p") 'tkb-next-blank-line))

(progn
  (defun tkb-yank-dired-filename ()
    (interactive)
    (let ((filename (dired-get-filename)))
      (kill-new filename)))
  (eval-after-load "dired"
    '(define-key dired-mode-map "F" 'tkb-yank-dired-filename)))

;; use ucs-insert to insert unicode characters.

;; Copy the current region and kill it escaped for common lisp.
(progn
  (defun tkb-copy-and-kill-for-common-lisp (beg end)
    (interactive "r")
    (let ((s (buffer-substring beg end))
          (buf (get-buffer-create " *crazy-yank-for-common-lisp*")))
      (save-excursion
        (set-buffer buf)
        (delete-region (point-min) (point-max))
        (princ s buf)
        (goto-char 0)
        (while (search-forward "\"" nil t)
          (backward-char)
          (insert ?\\)
          (forward-char 1))
        (kill-region (point-min) (point-max))
        ;;(kill-buffer buf)
        )))
  (tkb-keys ((kbd "C-c k L") 'tkb-copy-and-kill-for-common-lisp)))

(progn
  ;; FIXME: This should probably prompt for replacing???
  (defun fc-eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))
  (tkb-keys ((kbd "C-c E") #'fc-eval-and-replace)))

;;; FIXME: C-u C-x C-e aka eval-last-sexp does this, without the
;;; prompt to see if you want to insert.  I should look at its
;;; implementation to see if this could be better???
(defun tkb-eval-backward-and-insert (stringifyp)
  "Insert value of the preceding sexp."
  (interactive "P")
  (let (b
        e
        (output-format (if stringifyp "%s" "%S")))
    (save-excursion
      (backward-sexp)
      (setq b (point))
      (forward-sexp)
      (setq e (point)))
    (let ((xs (buffer-substring-no-properties b e)))
      (condition-case nil
          (let* ((x (read xs))
                 (result (eval x)))
            (when (y-or-n-p
                   (format (concat "Result: %S; Insert as "
                                   output-format "? ") result result))
              (insert (format output-format result))))
        (error (message "Invalid expression: %s" xs))))))

(defun tkb-eval-prompted-and-insert (stringifyp x)
  (interactive "P\nXLisp Expression: ")
  (let ((output-format (if stringifyp "%s" "%S")))
    (when (y-or-n-p
           (format (concat "Result: %S; insert as " output-format "? ")
                   x x))
      (insert (format output-format x)))))

(defun tkb-eval-backward-and-insert-in-comment (stringifyp)
  "Insert value of the preceding sexp."
  (interactive "P")
  (let (b
        e
        (output-format (if stringifyp ";;=> %s" ";;=> %S")))
    (save-excursion
      (backward-sexp)
      (setq b (point))
      (forward-sexp)
      (setq e (point)))
    (let ((xs (buffer-substring-no-properties b e)))
      (condition-case nil
          (let* ((tkb-ebaiicx (read xs))
                 (result (eval tkb-ebaiicx)))
            (when (y-or-n-p
                   (format (concat "Result: %S; Insert as "
                                   output-format "? ") result result))
              (insert (format output-format result))))
        (error (message "Invalid expression: %s" xs))))))

(when nil
  ;; http://emacsredux.com/blog/2013/06/21/eval-and-replace/
  ;; but see fc-eval-and-replace
  (defun eval-and-replace ()
    "Replace the preceding sexp with its value."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0))))))


(defun tkb-trim-buffer ()
  (interactive)
  (beginning-of-buffer)
  (save-excursion
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil))))
;;(tkb-keys ((kbd "C-c T") 'tkb-trim-buffer))

(defun tkb-trim-region ()
  (interactive)
  (save-excursion
    (let ((start (min (point) (mark)))
          (end (max (point) (mark))))
      (goto-char start)
      (while (re-search-forward "[ \t]+$" end t)
        (replace-match "" nil nil)))))

(progn
  (defun tkb-replace-with-calc (start end)
    (interactive "r\n")
    (let ((o (make-overlay start end)))
      (overlay-put o 'face 'highlight)
      (unwind-protect
          (let* ((n (string-to-number (buffer-substring start end)))
                 (x (read-from-minibuffer (format "Expression with n = %s: " n)
                                          nil read-expression-map t
                                          'read-expression-history
                                          )))
            (kill-region start end)
            (let ((r (number-to-string (eval x))))
              (message "Result: %s" r)
              (insert r)))
        (delete-overlay o))))
  (when nil (tkb-keys ((kbd "C-c e") #'tkb-replace-with-calc))))

(progn
  ;; http://emacs.wordpress.com/2007/01/20/record-play-re-play/
  (tkb-keys
    ([f7]  'start-kbd-macro)
    ([f8]  'end-kbd-macro)
    ([f9]  'call-last-kbd-macro)))


;;; Connects as sysdba.
(defun tkb-oracle-sysdba ()
  (interactive)
  (let ((sql-user "sys")
                                        ;(sql-database "nspcp")
        (sql-oracle-options (list "as sysdba")))
    (sql-oracle)))

(progn
  (defun tkb-next-sexp ()
    (interactive)
    (backward-up-list -1))

  (tkb-keys ((kbd "C-c )") 'tkb-next-sexp)))


(progn
  (defun tkb-copy-buffer-file-name (whole)
    (interactive "P")
    (message "whole: %s numeric: %d" whole (prefix-numeric-value whole))
    (let ((fn (buffer-file-name)))
      (kill-new (if whole fn (file-name-nondirectory fn)))))
  (tkb-keys ((kbd "C-c b f") #'tkb-copy-buffer-file-name))
  (defun tkb-copy-downcase-buffer-file-name (whole)
    (interactive "P")
    (message "whole: %s numeric: %d" whole (prefix-numeric-value whole))
    (let* ((fn (buffer-file-name))
           (fn (if whole fn (file-name-nondirectory fn))))
      (kill-new (downcase fn))))
  (tkb-keys ((kbd "C-c b D") #'tkb-copy-downcase-buffer-file-name))
  (defun tkb-copy-upcase-buffer-file-name (whole)
    (interactive "P")
    (message "whole: %s numeric: %d" whole (prefix-numeric-value whole))
    (let* ((fn (buffer-file-name))
           (fn (if whole fn (file-name-nondirectory fn))))
      (kill-new (upcase fn))))
  (tkb-keys ((kbd "C-c b U") #'tkb-copy-upcase-buffer-file-name))

  (defun tkb-copy-buffer-name ()
    (interactive)
    (kill-new (buffer-name)))
  (tkb-keys ((kbd "C-c b n") #'tkb-copy-buffer-name))

  (defun tkb-buffer-name-to-register (register)
    (interactive "cBuffer name to register: ")
    (set-register register (buffer-name)))
  (tkb-keys ((kbd "C-c b r") #'tkb-buffer-name-to-register))

  (defun tkb-copy-buffer-basename ()
    (interactive)
    (kill-new (file-name-base buffer-file-name)))
  (tkb-keys ((kbd "C-c b b") #'tkb-copy-buffer-basename))

  (defun tkb-insert-buffer-basename ()
    (interactive)
    (insert (file-name-base buffer-file-name)))
  (tkb-keys ((kbd "C-c b B") #'tkb-insert-buffer-basename))

  )

(defun tkb-lower-to-register (register start end)
  (interactive "cLowercase to register: \nr")
  (set-register register (downcase (buffer-substring start end))))
(tkb-keys ((kbd "C-c *") 'tkb-lower-to-register))



(progn
  ;; http://emacs.wordpress.com/2007/01/22/killing-yanking-and-copying-lines/

  (defun jao-copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring"
    (interactive "p")
    (kill-ring-save (line-beginning-position)
                    (line-beginning-position (+ 1 arg)))
    (message "%d line%s copied" arg (if (= 1 arg) "" "s")))


  (defadvice yank (after indent-region activate)
    (if (member major-mode '(emacs-lisp-mode scheme-mode lisp-mode
                                             c-mode c++-mode objc-mode
                                             LaTeX-mode TeX-mode))
        (indent-region (region-beginning) (region-end) nil)))
  (tkb-keys ((kbd "C-c y") 'jao-copy-line))
  )




(defun tkb-ispell-selection ()
  (interactive)
  (let ((word (x-get-selection-value)))
    (if word
        (let ((buf (get-buffer-create "Spell Selection")))
          (switch-to-buffer buf)
          (delete-region (point-min) (point-max))
          (insert word)
          (ispell-word))
      (message "no selection"))))
(tkb-keys ((kbd "C-c $") (if (fboundp 'x-get-selection-value)
                             #'tkb-ispell-selection
                           #'(lambda () (interactive)
                               (message "\
Not under a window system, so you can't ispell the selection")))))

(defun tkb-ispell-prompted-word (word)
  (interactive "sWord to check spelling? ")
  (let ((buf (get-buffer-create "Spell Word")))
    (switch-to-buffer buf)
    (delete-region (point-min) (point-max))
    (insert word)
    (ispell-word)))
(tkb-keys ((kbd "C-c %") #'tkb-ispell-prompted-word))

(when nil
  ;; Requires w3
  ;; http://www.emacswiki.org/cgi-bin/wiki/chmouel
  ;; Search Google at point:
  (defun my-search-google (w)
    "Launch google on the Word at Point"
    (interactive
     (list (let* ((word (thing-at-point 'symbol))
                  (input
                   (read-string (format "Google%s: "
                                        (if (not word)
                                            ""
                                          (format " (default %s)" word))))))
             (if (string= input "")
                 (if (not word)
                                        ;sinon input
                     (error "No keyword to search given") word) input))))
    (browse-url (format "http:/www.google.com/search?q=%s" w)))
  (tkb-keys ((kbd "C-c j") #'my-search-google)))


(when nil
  (eval-after-load "rng-loc"
    '(progn
       (setq rng-schema-locating-files
             (append rng-schema-locating-files
                     '("~/comp/xsl-website/website-schemas.xml"))))))

(when t
  ;; This requires using message-user-agent for composing mail.
  (setq mail-user-agent 'message-user-agent)
  (setq message-from-style 'angles)
  (require 'smtpmail)
  (setq smtpmail-debug-info t)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)
  (setq message-mail-alias-type 'ecomplete)
  (defun check-attachments-attached ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* (
             ;; Nil when message came from outside (eg calling emacs as editor)
             ;; Non-nil marker of end of headers.
             (internal-messagep
              (re-search-forward
               (concat "^" (regexp-quote mail-header-separator) "$") nil t))
             (end-of-headers		; Start of body.
              (copy-marker
               (or internal-messagep
                   (re-search-forward "^$" nil t)
                   (point-min))))
             (limit
              (or (re-search-forward "^-- $" nil t)
                  (point-max)))
             (old-case-fold-search case-fold-search))
        (unwind-protect
            (progn
              (goto-char end-of-headers)
              (when (search-forward "attach" limit t)
                (goto-char end-of-headers)
                ;; the word 'attach' has been used, can we find an
                ;; attachment?
                (unless
                    (or (re-search-forward "^<#/" limit t)
                        (y-or-n-p
                         "Found the word `attach' but no MIME attachment: send anyway?"
                         )
                        (error "Aborted send")))))
          (set-marker end-of-headers nil)))))
  (add-hook 'message-send-hook 'check-attachments-attached)
  (when nil 
    (defun tkb-messsage-header-setup-hook ()
      (goto-char (point-max))
      (insert "BCC: tkurtbond@gmail.com\n"))
    (setq message-header-setup-hook #'tkb-messsage-header-setup-hook)))

(when t
  (defun tkb-zap-to-char (arg char)
    "Kill up to and including ARG'th occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
    (interactive "p\ncZap to char: ")
    (kill-region (point) (progn
                           (search-forward (char-to-string char) nil nil arg)
                           (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
                           (point))))
  (tkb-keys ((kbd "C-c Z") 'tkb-zap-to-char)))


(when (eq locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(when nil
  ;; http://www.tbray.org/ongoing/When/200x/2003/09/27/UniEmacs
  (progn
    (defvar ongoing-char-choice
      '("Special characters"
        (""
         ("ccedil"    #xe7)
         ("copyright" #xa9)
         ("degree"    #xb0)
         ("dot"       #xb7)
         ("eacute"    #xe9)
         ("half"      "&#xbd;")
         ("omacr"     "&#x14d;")
         ("oouml"     #xe4)
         ("uuml"      #xfc)
         ("euro"      #x20ac)
         ("cents"     #xa2)
         ("egrave"    #xe8)
         ("lsquo"     #x2018)
         ("rsquo"     #x2019)
         ("ldquo"     #x201c)
         ("rdquo"     #x201d)
         ("mdash"     #x2014))))

    (defun ong-special-chars-menu ()
      "Insert a special character from a menu"
      (interactive)
      (let ((value
             (car (x-popup-menu
                   (list '(10 10) (selected-window))
                   ongoing-char-choice))))
        (cond
         ((integerp value) (ucs-insert value))
         ((stringp  value) (insert value))
         ('t )))) ;; so you can hit escape and make the menu go away


    (defun one-quote () "" (interactive) (insert ?'))
    (defvar sq-state 'nil "In single-quotes?")
    (defvar dq-state 'nil "In double quotes?")
    (defun ong-insert-special (c) "Insert special characters, like so:
 s => open/close single quotes
 d => open/close double quotes
 ' => apostrophe
 a => <a href=
 i => <img src=
 & => &amp;
 < => &lt;
 - => mdash
 . => center-dot"
      (interactive "c" "'")
      (cond
       ((= c ?s)
        (if sq-state
            (progn
              (ucs-insert #x2019)
              (setq sq-state 'nil))
          (ucs-insert #x2018)
          (setq sq-state 't)))
       ((= c ?d)
        (if dq-state
            (progn
              (ucs-insert #x201d)
              (setq dq-state 'nil))
          (ucs-insert #x201c)
          (setq dq-state 't)))
       ((= c ?') (ucs-insert #x2019))
       ((= c ?a)
        (progn
          (if (> (current-column) 0) (newline-and-indent))
          (insert "<a href=\"\">")
          (backward-char 2)
          ))
       ((= c ?i)
        (progn
          (if (> (current-column) 0) (newline-and-indent))
          (insert "<img src=\"\" alt=\"\" />")
          (backward-char 11)
          ))
       ((= c ?&) (insert "&amp;"))
       ((= c ?<) (insert "&lt;"))
       ((= c ?-) (ucs-insert #x2014))
       ((= c ?.) (ucs-insert #xb7))))))


(when nil
  ;; From Daniel Barlow: http://ww.telent.net/diary/2003/1/#14.28515
  ;; this works for gnus.
  (defun check-attachments-attached ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* (
             ;; Nil when message came from outside (eg calling emacs as editor)
             ;; Non-nil marker of end of headers.
             (internal-messagep
              (re-search-forward
               (concat "^" (regexp-quote mail-header-separator) "$") nil t))
             (end-of-headers		; Start of body.
              (copy-marker
               (or internal-messagep
                   (re-search-forward "^$" nil t)
                   (point-min))))
             (limit
              (or (re-search-forward "^-- $" nil t)
                  (point-max)))
             (old-case-fold-search case-fold-search))
        (unwind-protect
            (progn
              (goto-char end-of-headers)
              (when (search-forward "attach" limit t)
                (goto-char end-of-headers)
                ;; the word 'attach' has been used, can we find an
                ;; attachment?
                (unless
                    (or (re-search-forward "^<#/" limit t)
                        (y-or-n-p
                         "Found the word `attach' but no MIME attachment: send anyway?"
                         )
                        (error "Aborted send")))))
          (set-marker end-of-headers nil)))))

  (add-hook 'message-send-hook 'check-attachments-attached))

;; Never use separate frames for ediff.  (Separate frames are totally useless
;; on tiled, tabbing window managers like ion.)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(when-load-file "gforth.el"
  (autoload 'forth-mode "gforth")
  (add-to-list 'auto-mode-alist '("\\.fs\\'" . forth-mode))
  (autoload 'forth-block-mode "gforth")
  (add-to-list 'auto-mode-alist '("\\.fb\\'" . forth-block-mode))
  (add-hook 'forth-mode-hook #'(lambda ()
                                 ;; Why are we setting the following to the defaults?
                                 ;; customize variables here:
                                 (setq forth-indent-level 4)
                                 (setq forth-minor-indent-level 2)
                                 (setq forth-hilight-level 3)
                                 ))
  (setq forth-custom-indent-words
        '((("while" "[while]")
           (-2 . 2)
           (0 . 2))
          (("repeat" "[repeat]")
           (-2 . 0)
           (0 . -4))
          ))


  (eval-after-load "gforth"
    '(progn
      (load "tkb-forth")
       (defun forth-load-file (file-name)
         "Load a Forth file FILE-NAME into the inferior Forth process."
         (interactive (comint-get-source "Load Forth file: " forth-prev-l/c-dir/file
                                         forth-source-modes t)) ; T because LOAD
                                        ; needs an exact name
         (comint-check-source file-name) ; Check to see if buffer needs saved.
         (setq forth-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                             (file-name-nondirectory file-name)))
         (comint-send-string (forth-proc) (concat "include "
                                                  file-name
                                                  "\n"))))))


(defun tkb-insert-name ()
  (interactive)
  (insert "T. Kurt Bond"))

(defun tkb-insert-e-mail ()
  (interactive)
  (insert user-mail-address))

;; Should I use x-display-pixel-height and x-display-pixel-width to move to
;; an entirely emacs-based geometry munging?
(cond
 (nil
  ;; Make new frames show up exactly on top of the old frame.
  ;; This is especially useful with focus-follows-pointer mode
  ;; and scrolling desktops.
  (let* ((params (frame-parameters))
         (top (assq 'top params))
         (left (assq 'left params))
         (height (assq 'height params))
         (width (assq 'width params)))
    (message "%s %s %s %s" top left height width)
    ;; Setting these seems only to work well with twm and close derivatives;
    ;; other window managers seem to offset the new windows from the old.
    (push top default-frame-alist)
    (push left default-frame-alist)
    (push height default-frame-alist)
    (push width default-frame-alist)
    (push '(user-position . t) default-frame-alist)))
 (nil
  (let* ((height (assq 'height (frame-parameters)))
         (new-geometry `((top . 30)
                         (left . -75)
                         (width . 80)
                         ,height)))
    ;; Shouldn't I be replacing any matching items in the assoc?
    (cond (nil
           (setq initial-frame-alist new-geometry)
           (setq default-frame-alist (copy-list new-geometry)))
          (t
           (mapcar (lambda (item)
                     (push item initial-frame-alist)
                     (push item default-frame-alist))
                   new-geometry))))
  (tkb-position)))

(defun tkb-position-frame (&optional new-left)
  (interactive "P")
  (let* ((new-left (if new-left (prefix-numeric-value new-left) -50))
         (dw (display-pixel-width))
         (fw (frame-pixel-width))
         (nl (- dw fw 50))
         (top (frame-parameter nil 'top)))
    ;;(set-frame-position (selected-frame) nl top)
    ;;(set-frame-parameter nil 'left nl)
    ;;(frame-notice-user-settings)
    (set-frame-parameter nil 'left new-left)
    (set-frame-parameter nil 'top 20)
    (message "Yowza! %d %d %d %s" dw fw nl (current-time-string))))


(defun livejorunal-rss-url (name)
  (interactive "Mname:")
  (insert "\nhttp://www.livejournal.com/users/" name  "/data/rss\n"))

(defun livejorunal-rss-url (name)
  (interactive "MName:")
  (kill-new (concat  "http://www.livejournal.com/users/"
                     name  "/data/rss")))

(when (file-loadable-p "table")
  (autoload 'table-insert "table" nil t)
  (autoload 'table-recognize "table" nil t))


(when-load-file "rst"			;was when-load-dir
  (autoload 'rst-mode "rst"
    "mode for editing reStructuredText documents" t)
  (setq auto-mode-alist
        (append '(("\\.rst$" . rst-mode)
                  ("\\.rsti$" . rst-mode) ; include files.
                  ("\\.rest$" . rst-mode)
                  ("\\.rst-pending$" . rst-mode) ;for my blog.
                  )
                auto-mode-alist))
  (autoload 'rst-repeat-last-character "rst")
  (tkb-keys ((kbd "C-c R") 'rst-repeat-last-character))

  (defun tkb-rst-mode-hook ()
    (interactive)
    (setq indent-tabs-mode nil)
    (unless window-system
      ;; rst-mode fontlock is unreadable with the colors screen uses.
      (make-local-variable 'rst-mode-lazy)
      (make-local-variable 'font-lock-support-mode)
      (setq rst-mode-lazy nil
            font-lock-support-mode nil)
      (make-local-variable 'font-lock-mode)
      (font-lock-mode -1))
    ;;(message "tkb-rst-mode-hook ran; font-lock-mode: %S" font-lock-mode)
    ;;(tkb-smart-unicode-mode)        ; This got annoying after a while.
    ;;FIXME: makes all emacs hang???
    (when t (flyspell-mode 1))
    (when nil ;; Don't use this, since I mostly use pandoc now.
      (cond
       ((string-match "status-.+\\.rst$" buffer-file-name)
        ;; Don't set compile-command.
        nil)
       ((string-match ".*/myblog/entries/.*\\.\\(rst\\|rsti\\)\\(-pending\\)?$" buffer-file-name)
        (message "tkb-rst-mode-hook: matched myblog")
        (set (make-local-variable 'compile-command)
             (concat "pybloxrst "
                     (file-name-nondirectory buffer-file-name)
                     " >~/tmp/x.rst && rst -o -h ~/tmp/x.rst")))
       (t
        (message "didn't match myblog")
        (let* ((rst-name (file-name-nondirectory buffer-file-name))
               (ltx-name (concat (file-name-sans-extension rst-name) ".ltx"))
               (pdf-name (concat (file-name-sans-extension rst-name) ".pdf")))
          (set (make-local-variable 'compile-command)
               (if (file-exists-p "rststyle.tex")
                   (format "make %s && open %s" pdf-name pdf-name)
                 (concat "rst -o -p " rst-name)))))
       (add-hook 'before-save-hook 'time-stamp nil t))))
  (add-hook 'rst-mode-hook 'tkb-rst-mode-hook))

(progn
  (push '("\\.adc$" . adoc-mode)  auto-mode-alist)
  (push '("\\.adoc$" . adoc-mode)  auto-mode-alist)
  (defun tkb-adoc-mode-hook ()
    (tkb-smart-unicode-mode))
  (add-hook 'adoc-mode-hook #'tkb-adoc-mode-hook))

(setq Info-scroll-prefer-subnodes nil) ;scroll to end before jumping to subnodes

(when nil ;; not using this any more
  (file-loadable-p "cpb/personal-log")
  (setq personal-log-dir "~/current")

  (load "cpb/personal-log")
  (defun tkb-eulisp-log (read)
    (interactive "P")
    (let ((personal-log-file (expand-file-name "working-notes.text"
                                               "~/comp/eulisp")))
      (personal-log read)))

  (defun tkb-private-log (read)
    (interactive "P")
    (let ((personal-log-dir "~/tkb/"))
      (let ((personal-log-file (personal-log-calc-file)))
        (personal-log read))))

  (tkb-keys
    ((kbd "C-c l e") 'tkb-eulisp-log)
    ((kbd "C-c l h") 'personal-log-here)
    ((kbd "C-c l P") 'tkb-private-log)
    ((kbd "C-c l p") 'personal-log)))


(defun tkb-dec-to-hex (n)
  (interactive "NDec: ")
  (message "0x%x" n))
(defun tkb-hex-to-dec (n)
  (interactive "sHex: ")
  (message "%d" (string-to-number n 16)))
(tkb-keys
  ((kbd "C-c H") 'tkb-dec-to-hex)
  ((kbd "C-c D") 'tkb-hex-to-dec))



(defun tkb-kill-excess ()
  (interactive)
  (beginning-of-line)
  (while (not (eobp))
    (if (not (looking-at "^\\(.*\\)\\.zip"))
        (kill-line 1)
      (forward-line))))

(defun tkb-insert-rpg-net-post-url (post-id)
  (interactive "sPost Id: ")
  (insert (format "http://forum.rpg.net/showthread.php?s=&postid=%s#post%s"
                  post-id post-id)))

(defun tkb-get-x-selection ()
  (let (text)
    (when x-select-enable-clipboard
      (if (null text)
          (condition-case c
              (setq text (x-get-selection 'CLIPBOARD 'COMPOUND_TEXT))
            (error nil)))
      (if (null text)
          (condition-case c
              (setq text (x-get-selection 'CLIPBOARD 'STRING))
            (error nil)))
      (if (string= text "") (setq text nil)))

    ;; Don't die if x-get-selection signals an error.
    (if (null text)
        (condition-case c
            (setq text (x-get-selection 'PRIMARY 'COMPOUND_TEXT))
          (error nil)))
    (if (null text)
        (condition-case c
            (setq text (x-get-selection 'PRIMARY 'STRING))
          (error nil)))
    (if (string= text "") (setq text nil))

    (or text (setq text (x-get-cut-buffer 0)))
    (if (string= text "") (setq text nil))

    text))

(defun tkb-snarf-rpg-net-url ()
  (interactive)
  (let ((text (tkb-get-x-selection)))
    (message "text: %S" text)
    (when (and text (string-match "postid=\\([0-9]+\\)" text))
      (message "(match-string 1): %S" (match-string 1 text))
      (tkb-insert-rpg-net-post-url (match-string 1 text)))))

(defun tkb-insert-url-quoted (c)
  (interactive "cCharacter:")
  (insert (format "%%%x" c)))
(defun tkb-substitute-url-quoted ()
  (interactive)
  (let* ((cs (buffer-substring-no-properties (point) (+ (point) 1)))
         (c (aref cs 0)))
    (kill-region (point) (+ (point) 1))
    (tkb-insert-url-quoted c)))

(defvar tkb-saved-buffer-name nil)
(defun tkb-save-buffer-name ()
  (interactive)
  (setq tkb-saved-buffer-name (buffer-file-name)))
(defun tkb-insert-saved-buffer-name ()
  (interactive)
  (insert tkb-saved-buffer-name))
(tkb-keys ((kbd "C-c s") 'tkb-save-buffer-name))
(tkb-keys ((kbd "C-c S") 'tkb-insert-saved-buffer-name))

(defun tkb-unwebify-region (p m)
  (interactive "r")
  (let ((s (buffer-substring-no-properties p m)))
    (message "%d %d s: %s" p m s)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (while (search-forward-regexp "%\\([0-9a-f][0-9a-f]\\)" nil t)
        (let ((v (match-string 1)))
          (replace-match (char-to-string (string-to-number v 16)))))
      (copy-region-as-kill (point-min) (point-max)))))



(defun tkb-make-marker (&optional pos insertion-type)
  "Return a marker that points to position POS with insertion type
 INSERTION-TYPE."
  (unless pos (setq pos (point)))
  (let ((m (make-marker)))
    (set-marker m pos)
    (when insertion-type
      (set-marker-insertion-type m t)) ; marker stays after inserted text.
    m))

(defvar tkb-yank-place nil)

(defun tkb-mark-yank-place ()
  "Mark a specific place in a buffer to yank to later."
  (interactive)
  (tkb-clear-yank-place)
  (setq tkb-yank-place (tkb-make-marker nil t))
  (message "tkb-yank-place set"))

(defun tkb-clear-yank-place ()
  (when tkb-yank-place
    (set-marker tkb-yank-place nil)
    (setq tkb-yank-place nil)))

(defun tkb-yank-at-place ()
  "Yank at a specific place in a buffer."
  (interactive)
  (if (not tkb-yank-place)
      (message "No yank place set")
    (goto-char tkb-yank-place)
    (yank)))

(tkb-keys ((kbd "C-M-y") #'tkb-yank-at-place)
          ((kbd "C-c C-@") #'tkb-mark-yank-place))

(defun tkb-quote-for-elisp (beg end)
  (interactive "r")
  (kill-new (format "%S" (buffer-substring-no-properties beg end))))


(defun tkb-w32-select-font ()
  ;; hacked from w32-win.el:(mouse-set-font &rest fonts)
  (if w32-use-w32-font-dialog
      (let ((chosen-font (w32-select-font (selected-frame)
                                          w32-list-proportional-fonts)))
        (and chosen-font (list chosen-font)))
    (x-popup-menu
     last-nonmenu-event
     ;; Append list of fontsets currently defined.
     ;; Conditional on new-fontset so bootstrapping works on non-GUI compiles
     (if (fboundp 'new-fontset)
         (append w32-fixed-font-alist (list (generate-fontset-menu)))))))

(defun tkb-w32-copy-select-font (first-only)
  (interactive "P")
  (message "%S" first-only)
  (let* ((fonts (tkb-w32-select-font))
         (fonts (if first-only (car fonts) fonts)))
    (kill-new (format "%S" fonts))))

(progn
  (defun is-type (obj &rest typesyms)
    (memq (type-of obj) typesyms))

  (defun tkb-edit-form (sym)
    (interactive (list (intern (completing-read "symbol? " obarray))))
    (let* ((value (eval sym))
           (quote-string
            (cond ((null value) nil)
                  ((is-type value 'integer 'float 'string) nil)
                  ((is-type value 'cons) 'quote))))
      ;; Emacs lisp needs a better pretty printer.
      (pp `(setq ,sym ,(if quote-string
                           `(,quote-string ,value)
                         `,value))
          (current-buffer)))))


(defalias 'λ #'lambda)			;this doesn't work right.

;;; ??? Needs finished; see describe-variable and describe-symbol.
(defun describe-structure (structure)
  "Display the full documentation of STRUCTURE (a symbol)."
  (interactive
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read (if (symbolp v)
                                    (format
                                     "Describe structure (default %s): " v)
                                  "Describe variable: ")
                                obarray
                                (lambda (vv)
                                  (or (get vv 'structure-documentation)
                                      (and (boundp vv) (not (keywordp vv)))))
                                t nil nil
                                (if (symbolp v) (symbol-name v))))
     (list (if (equal val "") v (intern val))))))

(defun t:get-hostname-from-http ()
  (interactive)
  (let* ((s (x-get-selection))
         (s* (progn
               (string-match "\\(https?://\\|file:///\\)\\([^/]+\\)" s)
               (match-string 2 s))))
    (kill-new s*)))
(tkb-keys ((kbd "C-c k C") #'t:get-hostname-from-http))

(defun t:get-directory-for-download-from-http ()
  "Hmmm.  a (non)work(ing) in progress."
  (interactive)
  (let* ((s (x-get-selection))
         (s1 (progn
               (string-match "\\(https?://\\|file:///\\)\\(.+\\)$" s)
               (match-string 2 s)))
         (s2 (substitute ?- ?/ s1))
         (s3 (replace-regexp-in-string "--?" "-" s2))
         (s4 (replace-regexp-in-string "-$" ""
                                       (replace-regexp-in-string "^-" "" s3))))
    s3))

(when-exec-found (gls "gls")
  (setq insert-directory-program gls))

(defun tkb-path-get (&optional env-variable)
  (let* ((env-variable (if env-variable env-variable "PATH"))
         (parts (getenv env-variable)))
    (if parts
        (split-string parts ":")
      "")))

(defun tkb-path-set (path-elements &optional env-variable)
  (let ((env-variable (if env-variable env-variable "PATH")))
    (setenv env-variable (mapconcat #'identity path-elements ":"))))

(defun tkb-path-prepend (directories &optional env-variable)
  (let ((path (tkb-path-get env-variable)))
    (cl-loop for dir in (reverse directories)
          do (unless (member* dir path :test #'string-equal)
               (push dir path)))
    (tkb-path-set path env-variable)))

(defun tkb-path-delete (directories &optional env-variable)
  (let ((path (tkb-path-get env-variable)))
    (cl-loop for dir in directories
          do (setq path (delete* dir path :test #'string-equal)))
    (tkb-path-set path env-variable)))

(defun tkb-path-append (directories &optional env-variable)
  (tkb-path-set (append (tkb-path-get env-variable) directories) env-variable))

(defun buffer-local-set-key (key func)
  ;; From: https://www.emacswiki.org/emacs/BufferLocalKeys
  (interactive "KSet key on this buffer: \naCommand: ")
  (let* ((mode-name (format "%s-magic" (buffer-name)))
         (name (intern mode-name))
         (map-name (format "%s-map" mode-name))
         (map (intern map-name)))
    (eval
     `(define-minor-mode ,name
        ,(concat
          "Automagically built minor mode to define buffer-local keys.\n"
          "\\{" map-name "}")
        nil " Editing"))
    (unless (boundp map)
      (set map (make-sparse-keymap)))
    ;; (eval
    ;;  `(define-key ,map ,key ',func))
    (define-key map key func)
    (funcall name t)))

(defun buffer-local-set-key (key func)
  ;; From: https://www.emacswiki.org/emacs/BufferLocalKeys
  (interactive "KSet key on this buffer: \naCommand: ")
  (let* ((mode-name (format "%s-magic" (buffer-name)))
         (name (intern mode-name))
         (map-name (format "%s-map" mode-name))
         (map (intern map-name)))
    (unless (boundp map)
      (set map (make-sparse-keymap)))
    (eval
     `(define-minor-mode ,name
        ,(concat
          "Automagically built minor mode to define buffer-local keys.\n"
          "\\{" map-name "}")
        nil " Editing" ,map))
    (eval
     `(define-key ,map ,key ',func))
    (funcall name t)))

(progn
  ;; There has got to be a better way to pass the name of path variable to use.
  (defvar tkb-edit-path-path-var nil
    "Name of path variable to edit.")

  (defun tkb-save-path ()
    (interactive)
    (let* ((path-var (if tkb-edit-path-path-var
                         tkb-edit-path-path-var
                       "PATH"))
           (s (buffer-substring-no-properties (point-min) (point-max)))
           (path (-remove (lambda (path-element)
                            (string-match "^[ \t]*$" path-element))
                          (s-split "\n" s))))
      (kill-buffer-and-window)
      (message "Setting path \"%s\" to \"%s\"" tkb-edit-path-path-var
               (tkb-path-set path tkb-edit-path-path-var))
      (setq tkb-edit-path-path-var nil)))

  (defun tkb-not-save-path ()
    (interactive)
    (kill-buffer-and-window)
    (setq tkb-edit-path-path-var nil))

  (defun tkb-edit-path (prefix)
    (interactive "P")
    (let* ((path-var (if prefix
                         (read-string "Path variable? ")
                       "PATH"))
           (path (tkb-path-get path-var))
           (buf (get-buffer-create "*Editing-PATH*")))
      (setq tkb-edit-path-path-var path-var)
      (pop-to-buffer buf '(display-buffer-pop-up-window))
      (delete-region (point-min) (point-max))
      (cl-loop for dir in path do (progn (insert dir) (insert "\n")))
      (buffer-local-set-key (kbd "C-c C-c") 'tkb-save-path)
      (buffer-local-set-key (kbd "C-c C-k") 'tkb-not-save-path)
      (message "Use C-c C-c to finish and set your path, or C-c C-k to abort.")))
  )

(defun tkb-prepend-to-path (directory env-variable)
  "Read a directory into DIRECTORY and if prefix arg in ENV-VARIABLE is
present read a string into ENV-VARIABLE and if not default ENV-VARIABLE to PATH,
then prepend DIRECTORY to the path in the environment ENV-VARIABLE."
  (interactive "DDirectory to add to environment variable at start? \nP")
  (let ((directory (expand-file-name directory))
        (env-variable
         (if env-variable (read-string "Environment Variable? ") "PATH")))
    (message "Directory: %s" directory)
    (tkb-path-prepend (list directory) env-variable)
    (message "%s=%s" env-variable (getenv env-variable))))

(defun tkb-append-to-path (directory env-variable)
  "Read a directory into DIRECTORY and if prefix arg in ENV-VARIABLE is
present read a string into ENV-VARIABLE and if not default ENV-VARIABLE to PATH,
then append DIRECTORY to the path in the environment ENV-VARIABLE."
  (interactive "DDirectory to add to environment variable at end? \nP")
  (let ((directory (expand-file-name directory))
        (env-variable
         (if env-variable (read-string "Environment Variable? ") "PATH")))
    (message "Directory: %s" directory)
    (tkb-path-append (list directory) env-variable)
    (message "%s=%s" env-variable (getenv env-variable))))


;;; ¿¿¿Doesn't handle single word links???
(defun t:extract-targets (start end)
  (interactive "r")
  (save-excursion
    (let (refs)
      (goto-char start)
      (save-restriction
        (narrow-to-region start end)
        ;; (cl-flet ((noprop (s) (set-text-properties 0 (length s) nil s))))???
        (while (re-search-forward "\\(`[^`]+`\\)\\(_+\\)" nil t)
          (message "point is %d" (point))
          (let ((name  (match-string 1))
                (under (match-string 2)))
            (push (format "\n.. %s %s: " under name) refs))))
      (goto-char end)
      (insert "\n\n" (apply #'concat refs)))))

(defun zap-upto-char (arg char)
  ;; From emacs-18.59.
  "Kill up to (but not including) ARG'th occurrence of CHAR.
Goes backward if ARG is negative; goes to end of buffer if CHAR not found."
  (interactive "*p\ncZap to char: ")
  (kill-region (point) (if (search-forward (char-to-string char) nil t arg)
                           (progn (goto-char (if (> arg 0) (1- (point)) (1+ (point))))
                                  (point))
                         (if (> arg 0) (point-max) (point-min)))))
(tkb-keys ((kbd "C-c k z") #'zap-upto-char))

(defun tkb-vms-tape-blocks/hour (start-block start-time end-block end-time)
  (interactive "nStart block? \nnEnd block? \nsStart Time? \nsEnd Time? ")
  (let* ((hours    (t:time-diff start-time end-time))
         (blocks   (- end-block start-block))
         ;; BACKUP$DATA:[BACKUP]8MM_BACKUP.COM has /BLOCK=32528, but
         ;; BACKUP Ctrl-T reports 32768 byte blocks
         (bytes    (* blocks 32768))
         (meg      (/ bytes 1024.0 1024.0))
         (meg/hr   (/ meg hours))
         (seconds  (* hours 60 60))
         (bytes/s  (/ bytes seconds))
         (bytes/s* (/ (* meg/hr 1024 1024) 60 60))
         (ans    `(hours
                   ,hours blocks ,blocks bytes ,bytes meg ,meg meg/hr ,meg/hr
                   bytes/s ,bytes/s bytes/s* ,bytes/s*)))
    (message "%S" ans)
    ans))

(when nil
  (tkb-vms-tape-blocks/hour 6 "12:50p" 31901 "14:20p")
  )



(defun d4   () (1+ (random   4)))
(defun d6   (num-dice)
  (interactive "p")
  (cl-loop for i from 1 to num-dice
        for roll = (1+ (random   6)) then (1+ (random   6))
        collect roll into rolls
        sum roll into result
        finally do (message "%dd6: %d rolls: %S" num-dice result rolls)
        finally return result))
(defun d8   () (1+ (random   8)))
(defun d10  () (1+ (random  10)))
(defun d12  () (1+ (random  12)))
(defun d20  () (1+ (random  20)))
(defun d100 () (1+ (random 100)))
(defun dF   () (- (1+ (random 3)) 2))

(defun dice (sides &optional number op mod repeat)
  "Roll dice, returning a list.
SIDES and must be an integer or the string \"F\".
NUMBER is the number of times to roll and add.
OP is the operation to perform: + - * h l.
MOD is the modifier for the op.
REPEAT is how many times to repeat the roll."
  (error "dice is not yet implemented"))

(defun d (sides number mod repeat)
  (interactive "nSides: \nnNumber: \nnMod: \nnRepeat: ")
  (cl-loop repeat repeat
        collect (cl-loop repeat number
                      sum (1+ (random sides)) into roll
                      finally return (+ roll mod))
        into rolls
        finally (insert (mapconcat #'int-to-string rolls ", "))))

(load-library "iso-transl.el")
;; This sticks keys in iso-transl-ctl-x-8-map, which makes them available under
;; the "C-x 8" key prefix.
;; Note that section and pilcrow are in C-x 8: S and P.

(iso-transl-define-keys '(;; Dingbats
                          ("dC" . [?✔])   ; HEAVY CHECK MARK
                          ("dc" . [?✓])  ; CHECK MARK
                          ("dd" . [?⯁])  ; black diamond centered
                          ("dl" . [?◊])  ; lozenge
                          ("dt" . [?▶])  ; black right pointing triangle
                          ("di" . [?☛])  ; black right pointing index
                          ;; Greek
                          ("gl" . [?λ])  ; greek lowercase lambda
                          ("gL" . [?Λ])  ; greek uppercase lambda
                          ;; Punctuation
                          ("p " . [? ])  ; non-breaking space.
                          ("p*" . [?×])  ; multiply
                          ("p-" . [?−])  ; Minus sign
                          ("p/" . [?÷])  ; divide
                          ("pD" . [?‡])  ; double dagger
                          ("pP" . [?″])  ; double prime
                          ("pQ" . [?“])  ; open double quote
                          ("pS" . [?‘])  ; open single quote
                          ("pb" . [?•])  ; bullet
                          ("pd" . [?†])  ; dagger
                          ("pe" . [?…])  ; ellipsis
                          ("ph" . [?­])  ; soft hyphen
                          ("pm" . [?—])  ; M-dash
                          ("pn" . [?–])  ; N-dash
                          ("pp" . [?′])  ; prime
                          ("pq" . [?”])  ; close double quote
                          ("ps" . [?’])  ; close single quote
                          ("pv" . [?‖])  ; double vertical bar
                          ;; Gender
                          ("Gh" . [?×])  ; Hybrid gender in biology
                          ("Gm" . [?♂])  ; male sign
                          ("Gf" . [?♀])  ; female sign
                          ("GM" . [?⚣])  ; male homosexuality
                          ("GF" . [?⚢])  ; female homosexuality
                          ("Gb" . [?⚥])  ; male & female, both
                          ("Ga" . [?⚪])  ; agender, sexless
                          ("Gt" . [?⚧])  ; transgender
                          ;; Symbols
                          ("sc" . [?©])  ; copyright
                          ("sC" . [?🄯])  ; copyleft
                          ("sd" . [?Δ]) ; GREEK CAPITAL LETTER DELTA
                          ("si" . [?∞]) ; infinity
                          ("sp" . [?¶]) ; pilcrow
                          ("ss" . [?§]) ; SECTION SIGN in Unicode; also silcrow
                          ("sF" . [?℉]) ; Degrees Fahrenheit
                          ("sC" . [?℃]) ; Degrees Centigrade/Celsius
                          ("sK" . [?K]) ; Degrees Kelvin
                          ("sD" . [?°]) ; Degree sign
                          ("sx" . [?🗙]) ; Cancelation X
                          ;; Common or Vulgar Fractions
                          ("5/8" . [?⅝])
                          ("4/5" . [?⅘])
                          ("1/8" . [?⅛])
                          ("1/6" . [?⅙])
                          ("7/8" . [?⅞])
                          ("3/8" . [?⅜])
                          ("2/3" . [?⅔])
                          ("5/6" . [?⅚])
                          ("1/5" . [?⅕])
                          ("1/3" . [?⅓])
                          ("3/5" . [?⅗])
                          ("2/5" . [?⅖])
                          ))




(when-file (f "/sw/src/go/go/misc/emacs/go-mode-load.el")
  (add-to-list 'load-path (file-name-directory f) t)
  (require 'go-mode-load))




(when nil
  ;; https://www.reddit.com/r/emacs/comments/31xezm/common_byte_units_in_calc/
  (setq math-additional-units '(
                                (GiB "1024 * MiB" "Giga Byte")
                                (MiB "1024 * KiB" "Mega Byte")
                                (KiB "1024 * B" "Kilo Byte")
                                (B nil "Byte")
                                (Gib "1024 * Mib" "Giga Bit")
                                (Mib "1024 * Kib" "Mega Bit")
                                (Kib "1024 * b" "Kilo Bit")
                                (b "B / 8" "Bit")))

  ;; This resets calc's cache
  (setq math-units-table nil))

(progn
  ;; http://pragmaticemacs.com/emacs/uniquify-your-buffer-names/
  ;; meaningful names for buffers with the same name
  ;; from prelude
  ;; https://github.com/bbatsov/prelude
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)	; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  )

(when-exec-found (e "chez") (setq geiser-chez-binary e))
(when-exec-found (e "chicken-csi")
  (setq geiser-chicken-binary e)
  (message "geiser-chicken-binary is %s" geiser-chicken-binary))
(setq geiser-default-implementation 'chicken) ;; not sure why this doesn't work.

(defalias 'tkb-saved-geiser-chicken--external-help
  (symbol-function 'geiser-chicken--external-help))
(defun tkb-local-geiser-chicken--external-help (id _module)
  "Load chicken doc for ID into a buffer."
  (let* ((version (geiser-chicken--version (geiser-chicken--binary)))
         (major-version (car (split-string version "\\\."))))
    (browse-url (format "http://localhost:7001/cdoc?q=%s&query-name=Look+up"
                        id))))
(defun tkb-switch-chickadee ()
  (interactive)
  (if (eq (symbol-function 'geiser-chicken--external-help)
          (symbol-function 'tkb-saved-geiser-chicken--external-help))
      (defalias 'geiser-chicken--external-help
        'tkb-local-geiser-chicken--external-help)
    (defalias 'geiser-chicken--external-help
      'tkb-saved-geiser-chicken--external-help)))
    
(when-load-file "magit"
  :load
  (global-set-key (kbd "C-x M s") 'magit-status)
  (setq magit-diff-refine-hunk 'all)
  (when (version< emacs-version "27.0")
    (when (fboundp 'global-magit-file-mode)
      (global-magit-file-mode))
    (when (boundp 'magit-file-mode-map)
      (define-key magit-file-mode-map
        (kbd "C-x M g") 'magit-file-dispatch))))

(add-hook 'markdown-mode-hook 'flyspell-mode)

(defun t:insert-alphabet ()
  (interactive)
  (cl-loop for x from ?A to ?Z
        for y from ?a to ?z
        do (insert (format "%c%c" x y))))

(defun named-line (name)
  (let* ((line-len 79)
         (prefix-len 10))
    (concat (make-string prefix-len ?=) " " name " "
                       (make-string (- line-len prefix-len
                                       1 (length name) 1)
                                    ?=))))



(defun t:insert-lined-file (filename)
  (interactive "fFile to insert between lines: ")
  (let ((just-filename (file-name-nondirectory filename)))
    (insert (named-line just-filename) "\n")
    (insert (named-line (concat "End of " just-filename)) "\n")
    (push-mark)
    (forward-line -1)
    (insert-file-contents filename nil)))
(tkb-keys ((kbd "C-c F l") 't:insert-lined-file))

(defun t:yank-lined (lined-name)
  (interactive "P")
  (setq lined-name (if lined-name (read-from-minibuffer "Lined Name? ")
                     "Example"))
  (insert (named-line lined-name) "\n")
  (insert (named-line (concat "End of " lined-name)) "\n")
  (push-mark)
  (forward-line -1)
  (yank))
(tkb-keys ((kbd "C-c k y") #'t:yank-lined))

(load-file "~/lib/emacs/arrows/arrows.el") ;my port of cl-arrows

(defun tkb-sanitize-for-filename (string &optional down)
  "Clean up characters in STRING that aren't good for filenames."
  (cl-flet ((maybe-down (s)
              (if down (downcase s) s)))
    (->> string
         (replace-regexp-in-string "&" "and")
         (replace-regexp-in-string "'" "")
         (replace-regexp-in-string "[^-.a-z0-9]+" "-")
         (replace-regexp-in-string "-+" "-")
         (replace-regexp-in-string "-\\." ".")
         (replace-regexp-in-string "\\.-" ".")
         (replace-regexp-in-string "\\.+" ".")
         (replace-regexp-in-string "^-+" "")
         (replace-regexp-in-string "-+$" "")
         (maybe-down))))

(defun tkb-sanitize-kill-for-filename (string)
  "Clean up characters that aren't good for filenames in the top of the kill
ring and put the result on the top of the kill ring."
  ;; gets the kill or the system clipboard if it is new.
  (interactive (list (current-kill 0 t)))
  (let ((new-string (tkb-sanitize-for-filename string)))
    (message "Old string: %s\nNew string: %s" string new-string)
    (kill-new new-string)))
(tkb-keys ((kbd "C-c k S") 'tkb-sanitize-kill-for-filename))

(defun tkb-initialize-kill-for-filename (string)
  "Get the initial letters in the the words of the top of the kill ring,
concatenate them, and pu the result on the top of the kill ring."
  (interactive (list (current-kill 0 t)))
  (let* ((string (replace-regexp-in-string "'" "" string))
         (strings (split-string string "[-_:()\" \f\t\n\r\v]+" t "[ \t]+"))
         (new-string (map 'string (lambda (s) (aref s 0)) strings)))
    (message "Old string: %s\nNew String: %s" string new-string)
    (kill-new new-string)))
(tkb-keys ((kbd "C-c k I") 'tkb-initialize-kill-for-filename))

(defun tkb-prefix-iso-date-kill (string)
  "Add the ISO YYYY-MM-DD date followed by - as a prefix to the current kill
ring."
  (interactive (list (current-kill 0 t)))
  (let ((new-string (concat (format-time-string "%Y-%2m-%2d") "-" string)))
    (message "Old string: %s\nNew string: %s" string new-string)
    (kill-new new-string)))
(tkb-keys ((kbd "C-c k P") 'tkb-prefix-iso-date-kill))

(defun tkb-suffix-iso-date-kill (string)
  "Add the ISO YYYY-MM-DD date preceded by - as a suffix to the current kill
ring."
  (interactive (list (current-kill 0 t)))
  (let ((new-string (concat string "-" (format-time-string "%Y-%2m-%2d"))))
    (message "Old string: %s\nNew string: %s" string new-string)
    (kill-new new-string)))
(tkb-keys ((kbd "C-c k p") 'tkb-suffix-iso-date-kill))

(defun tkb-append-region-to-kill (string region)
  "Append region to current kill, separated by a -."
  (interactive (list (current-kill 0 t)
                     (buffer-substring-no-properties (point) (mark))))
  (let ((new-string (concat string "-" region)))
    (message "Old string: %s\nNew string: %s" string new-string)
    (kill-new new-string)))
(tkb-keys ((kbd "C-c k A") 'tkb-append-region-to-kill))

(defun tkb-insert-post-fragment ()
  "Insert a fragment into the current, defaulting to
~/Repos/tkurtbond.github.io/fragments/post.rst."
  (interactive)
  (let ((filename (expand-file-name
                   (read-file-name
                    "Post Fragment? "
                    "~/Repos/tkurtbond.github.io/fragments/" nil nil
                    "post.rst"))))
    (insert-file-contents filename)))
(global-set-key (kbd "C-c i p") 'tkb-insert-post-fragment)
(tkb-key-is-bound-to (kbd "C-c i p") 'tkb-insert-post-fragment)
(defun tkb-insert-fragment ()
  "Insert a fragment into the the current buffer, defaulting to the directory
~/current/fragments/."
  (interactive)
  (let ((filename (expand-file-name
                   (read-file-name
                    "Post Fragment? "
                    "~/current/fragments/" nil nil))))
    (insert-file-contents filename)))
(global-set-key (kbd "C-c i f") 'tkb-insert-fragment)
(tkb-key-is-bound-to (kbd "C-c i f") 'tkb-insert-fragment)

(progn
  (defvar tkb-define-word-word nil)

  (tkb-keys ((kbd "C-c W w") 'define-word)
            ((kbd "C-c W W") 'define-word-at-point))
  (defadvice define-word (around tkb-around-define-word activate)
    "Dynamically bind tkb-define-word-word to the word passed in."
    (let (
          ;;(url-request-extra-headers '(("User-Agent" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_5_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.63 Safari/537.36")))
          (url-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 11_5_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.63 Safari/537.36")
          )
      (setq tkb-define-word-word word)
      ad-do-it
      (setq tkb-define-word-word nil)))

  (defun tkb-display-define-word (results)
    (with-output-to-temp-buffer "*Define Word*"
      (when (boundp 'tkb-define-word-word)
        (let* ((headline (format "Definition of \"%s\""
                                 tkb-define-word-word))
               (underline (make-string (length headline) ?=)))
          (princ headline)
          (princ "\n")
          (princ underline)
          (princ "\n\n")))
      (princ results)))

  (setq define-word-displayfn-alist '((wordnik . tkb-display-define-word)
                                      (openthesaurus . tkb-display-define-word)
                                      (webster . tkb-display-define-word)))

  (setq define-word-limit 100)
  )


(defun dice-average (number-of-dice number-of-sides plus)
  "Figure out the average roll for NUMBER-OF-DICE with NUMBER-OF-SIDES,
optionally with a number PLUS added to the result specified with a prefix arg."
  (interactive "nNumber of dice? \nnNumber of sides? \nP")
  (unless plus (setq plus 0))
  (let ((average-roll (+ (* (/ (+ number-of-sides 1) 2.0)
                            number-of-dice)
                         plus)))
    (message "%dd%d+%d Average Roll: %g" number-of-dice number-of-sides plus
             average-roll)))


(defun height-mass (start-height start-mass end-height build-ratio)
  "How much does your giant weigh?
Calculate from START-HEIGHT, START-MASS, and END-HEIGHT multiplied by
BUILD-RATIO the END-MASS.  BUILD-RATIO is a factor to express the difference
in build between different creatures.  (The factor for a dwarf might be 2.25,
225% heavier than normal.)
Formula: end-mass = start-mass * (end-height / start-height)^3
See:
https://en.wikipedia.org/wiki/Square%E2%80%93cube_law
https://www.enworld.org/threads/how-much-does-my-giant-weight.106631/post-1846443"
  (interactive "nStarting Height? \nnStarting Mass? \nnEnd Height? \nnBuild Ratio? ")
  (let* ((start-height (* 1.0 start-height)) ; make measurements floats.
         (start-mass (* 1.0 start-mass))
         (end-height (* 1.0 end-height))
         (ratio (expt (/ end-height start-height) 3))
         (end-mass (* start-mass ratio)))
    (message "Start Height: %g; Start Mass: %g; End Height: %g; Build Ratio: %g
Ratio: %g; End Mass: %g; * Build Ratio: %g"
             start-height start-mass end-height build-ratio
             ratio end-mass (* build-ratio end-mass))))

(defun flesh-to-stone-weight (human-weight)
  "Calculate how much a human of HUMAN-WEIGHT pounds would weigh if they were
   converted into granite."
  (interactive "nHuman weight in pounds: ")
  (let* ((human-density 63.1)            ; lb/ft^3
         (granite-density 168)           ; lb/ft^3
         (granite-weight (* (/ human-weight human-density) granite-density)))
    (message "A human of %f pounds would weigh %f pounds
   if converted to granite."
             human-weight granite-weight)))

(defun tkb-remove-mod ()
  (interactive)
  (setq auto-mode-alist
        (remove-if (cl-function (lambda ((ext . mode))
                                  (cl-search ".mod" ext)))
                   auto-mode-alist)))

(defun tkb-unwrap-paragraphs (on-region)
  "Remove newlines between lines in paragraphs, but not newlines all by
themselves.  (The reverse of `fill-paragraph', \
\\<global-map>\\[fill-paragraph].)"
  (interactive "P")
  (query-replace-regexp "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2" nil
                        (if on-region (point) nil)
                        (if on-region (mark) nil)
                        nil nil))

(defun tkb-eval-insert (result)
  "Evaluate a Lisp expression and insert it."
  (interactive "XLisp Expression:")
  (insert (format "%s" result)))
(tkb-keys ((kbd "C-c k :") 'tkb-eval-insert))

(defun tkb-average-of-exploding-die (sides &optional user-p)
  (interactive "NNumber of sides? \np")
  (let* ((average (/ (+ 1.0 sides) 2.0))
         (wild (* average (/ (* 1.0 sides) (- sides 1)))))
    (when user-p
      (message "%d sides has an average of %g with a wild die average of %g"
               sides average wild))
    wild))
(tkb-keys ((kbd "C-c k C-e") 'tkb-average-of-exploding-die))

(defun tkb-mini-six-wild-roll-average (n)
  (let* ((n (- n 1))
         (avg (+ (tkb-average-of-exploding-die 6)
                 (* n 3.5))))
    avg))

(defun tkb-average-1D-to-14D-with-wild-die ()
  (interactive)
  (with-output-to-temp-buffer "*OpenD6 dice codes with averages*"
    (cl-loop for i from 1 to 14
          for avg = (tkb-mini-six-wild-roll-average i)
          then (tkb-mini-six-wild-roll-average i)
          do (princ (format "%dD with the wild die averages %g\n" i avg)))))

(defun tkb-hex-color-to-decimal (hexstring)
  "Convert a 6 digit hex color string to three decimal values for inputing in
inkscape."
  (interactive "S6 digit hex color string: ")
  (let ((r (substring hexstring 0 2))
        (g (substring hexstring 2 4))
        (b (substring hexstring 4 6)))
    (message "r: %d g: %d b: %d")))

(defun tkb-find-savage-worlds-attributes ()
  (interactive)
  (re-search-forward (rx (or "Agility" "Smarts" "Spirit" "Strength" "Vigor"))))

(defun tkb-save-buffer ()
  (interactive)
  (let* ((s (buffer-substring (point-min) (point-max)))
         (new-name (concat "Copy of " (buffer-name)))
         (new-buf (get-buffer-create new-name)))
    (switch-to-buffer new-buf)
    (insert s)))
(tkb-keys ((kbd "C-c b s") 'tkb-save-buffer))

(progn
  ;; https://www.emacswiki.org/emacs/UnfillRegion
  (defun unfill-region (beg end)
    "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
    (interactive "*r")
    (let ((fill-column (point-max)))
      (fill-region beg end)))

  ;; Handy key definition
  (define-key global-map "\C-\M-Q" 'unfill-region))

(defun tkb-points-to-inches (points)
  (interactive "NNumber of Points? ")
  (let ((inches (/ points 72.0)))
    (message "%f points is %f inches" points inches)))

(defun narrow-to-region-indirect (start end)
  ;; From https://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(when nil
  ;; For fiddling with/debugging man pages.
  (setq manual-program "man -C ~/lib/man.conf")
  (setq manual-program "man")
  (setq Man-switches "-C ~/lib/man.conf")
  (setq Man-switches "")
  (setq Man-fontify-manpage-flag t))

;; For some reason, when I pass a conf file that uses the version of
;; groff I've installed, man pages in emacs come out underlined.  I
;; guess the fontification doesn't deal well with the ANSI escapes the
;; current grotty outputs.  Anyway, I had to add -P-c to the NROFF
;; entry in my private version of man.conf to get it to use the old
;; style backspace bolding to get man pages in emacs to come out
;; right.  :( So now I have a ~/local/etc/man.conf to deal with local
;; variations on where I install groff.  And I've started using
;; ~/.bashrc_local-$(hostname -s) again to set up the proper alias for
;; man.  :(
(when nil 
  (when-exec-found (groff "groff" (list "/usr/local/sw/versions/groff/git/bin"
                                        "/sw/versions/groff/git/bin"))
    (message "Found a groff of ours: %s" groff)))

(when (file-exists-p "~/local/etc/man.conf")
  (setq Man-switches "-C ~/local/etc/man.conf"))

(defun tkb-sanitize-ats-and-uppercase ()
  ;; Abandoned for tkb-sanitize-ats-and-underscores.
  (interactive)
  (let* ((s (substring-no-properties (gui-get-selection 'PRIMARY 'STRING)))
         (s (cl-loop for c across s
                  with x = '()
                  if (or (<= ?A c ?Z) (= c ?@)) collect c into x
                  finally return (concat x))))
    (message "%s" s)
    (kill-new s)))

(defun tkb-sanitize-ats-and-underscores ()
  "Get selection and turn things like \"Aaa@An_Example_Title\" into \"A@AET\"
and make it the current selection."
  (interactive)
  (cl-labels ((sanitize (s)
                        (let ((start 0)
                              (len (length s))
                              acc)
                          (while (and (< start len)
                                      (string-match "\\([0-9a-z]+\\)" s start))
                            (push (aref  s (match-beginning 1)) acc)
                            (when (and (< (match-end 1) len)
                                       (= ?@ (aref s (match-end 1))))
                              (push ?@ acc))
                            (setq start (1+ (match-end 1))))
                          (concat (reverse acc)))))
    (let* ((s (substring-no-properties (gui-get-selection 'PRIMARY 'STRING)))
           (s (sanitize s)))
      (message "%s" s)
      (kill-new s))))
(global-set-key (kbd "C-c q") #'tkb-sanitize-ats-and-underscores)

(defun tkb-count-pages (start end)
  (interactive "r")
  (let* ((words (count-words start end))
         (pages (/ words 300.0)))
    (message "Words: %f Pages: %f" words pages)
    (cl-values words pages)))

(defun tkb-try (point mark)
  (interactive "r")
  (message "point: %d mark: %d" point mark))

(defun t:5-percent-xp (xp)
  (interactive "NXP? ")
  (let ((newxp (round (+ xp (* 0.05 xp)))))
    (message "XP %d + 5%% = %d" xp newxp)
    newxp))

(defun t:10-percent-xp (xp)
  (interactive "NXP? ")
  (let ((newxp (round (+ xp (* 0.1 xp)))))
    (message "XP %d + 5%% = %d" xp newxp)
    newxp))

(defun t:padleft (n)
  (interactive "NWidth? ")
  (let* ((s (cond ((< n 0)
                   (setq n (abs n))
                   (read-string "String? "))
                  (t (gui-get-selection 'PRIMARY 'STRING))))
         (s (s-pad-left n " " s)))
    (message "|%s|" s)
    (kill-new s)))

;; https://stackoverflow.com/a/20577329
(defun int-to-binary-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun fahrenheit-to (f)
  (interactive "NFahrenheit? ")
  (let* ((c (* (/ 5.0 9.0) (- f 32.0)))
         (k (+ 273.15 (* (/ 5.0 9.0) (- f 32.0))))
         (result (format "%6.2f℉ is %6.2f℃ and %6.2fK" f c k)))
    (message "%s" result)
    result))


(defun kelvin-to (k)
  (interactive "NKelvin? ")
  (let* ((f (+ (* 1.8 (- k 273)) 32))
         (c (- k 273))
         (result (format "%6.2fK is %6.2f℃ and %6.2f℉" k c f)))
    (message "%s" result)
    result))

(defun celsius-to (c)
  (interactive "NCelsius? ")
  (let* ((f (+ (* 1.8 c) 32))
         (k (+ c 273))
         (result (format "%6.2f℃ is %6.2f℉ and %6.2fK" c f k)))
    (message "%s" result)
    result))



(when nil
  ;; This is not the way to do it.
  (let ((pos (memq 'mode-line-modes mode-line-format)))
    (setcdr pos (cons (string-join (list (user-login-name) "@"
                                         (car (split-string (system-name) "\\." t))
                                         " "))
                      (cdr pos))))
  ;; Nor is this.
  (setq global-mode-string
        (concat" " (user-login-name) "@"
               (car (split-string (system-name) "\\." t))
               " ")))

(unless (getenv "EMACS_NO_MODE_STRING") ; mew blows up with this.
  (let ((user-at-host (concat" " (user-login-name) "@"
                             (car (split-string (system-name) "\\." t))
                             " ")))
    (if (member (user-login-name) '("root" "Administrator"))
        ;; Make it obvious if the user is root.
        (put-text-property 0 (length user-at-host) 'face
                           'error user-at-host)
      (put-text-property 0 (length user-at-host) 'face
                         'success user-at-host))
    (setq global-mode-string (list user-at-host))))

(global-set-key (kbd "C-c i k") 'string-inflection-kebab-case)
(global-set-key (kbd "C-c i C") 'string-inflection-capital-underscore)
(global-set-key (kbd "C-c i u") 'string-inflection-underscore)
(global-set-key (kbd "C-c i U") 'string-inflection-upcase)
(global-set-key (kbd "C-c i c") 'string-inflection-camelcase)
(global-set-key (kbd "C-c i l") 'string-inflection-lower-camelcase)
(global-set-key (kbd "C-c i j") 'string-inflection-java-style-cycle)
(global-set-key (kbd "C-c i n") 'string-inflection-cycle)

;; Adapt to the vishap oberon compiler's error message format.
(progn
  (setq vishap-oberon-compilation-error-alist
        '((vishap-oberon-file "^\\(\\w+\\.\\(Mod\\|obn\\|ob2\\)\\)  Compiling"
                              1 nil nil 0)
          (vishap-oberon-line "^\\s-+\\([0-9]+\\):"
                              nil 1 nil 2 nil)))

  (eval-after-load "compile"
    '(progn
       (setq compilation-error-regexp-alist
             (cons 'vishap-oberon-file
                   (cons 'vishap-oberon-line compilation-error-regexp-alist)))
       (setq compilation-error-regexp-alist-alist
             (append compilation-error-regexp-alist-alist
                     vishap-oberon-compilation-error-alist))))

  (defun update-for-vishap-oberon-compilation-error-alist ()
    "Use this when you change `vishap-oberon-compilation-error-alist'."
    (interactive)
    (cl-loop for item in vishap-oberon-compilation-error-alist
          do (progn
               (print (format "set to: %S\n" (cdr item)))
               (print (format "before: %S\n"
                              (-> (assoc (car item)
                                         compilation-error-regexp-alist-alist)
                                  (cdr))))
               (setf (-> (assoc (car item)
                                compilation-error-regexp-alist-alist)
                         (cdr))
                     (cdr item))
               (print (format "after:  %S\n" (-> (assoc (car item)
                                                        compilation-error-regexp-alist-alist)
                                                 (cdr))))
               ))))

(defvar tkb-w-map (make-sparse-keymap))
(global-set-key (kbd "C-c w") tkb-w-map)      ; Right now just asking various what questions.
(defun what-column ()
  "Display what column the cursor is in."
  (interactive)
  (message "Current Column: %d" (current-column)))
(define-key tkb-w-map "c" 'what-column)
(define-key tkb-w-map "l" 'what-line)


(defun tkb-toggle-groff-git ()
  (interactive)
  (let ((groff-git-info "/usr/local/sw/versions/groff/git/share/info"))
    (if (member groff-git-info Info-directory-list)
        (delete groff-git-info Info-directory-list)
      (add-to-list 'Info-directory-list groff-git-info))))


(defun tkb-open-nikola-post ()
  (interactive)
  ;; Apparently, emacs doesn't like raw ASCII NUL characters in a file when
  ;; you load it.  It doesn't care about it when you C-x C-e and expression....
  (if (looking-at (concat "\\.\\(/[0-9]+/[0-9]+/[0-9]+/.*\\)\\.rst"
                          (char-to-string 0)))
      (let* ((fragment (match-string-no-properties 1))
             (url (concat "http://0.0.0.0:8000/posts" fragment)))
        (browse-url url))
    (message "Huh.  No match.  Did you run the grep-find in the posts directory")))
(tkb-keys ((kbd "C-c k N") 'tkb-open-nikola-post))

(require 'elfeed-org)
(setq rmh-elfeed-org-files (list "~/.elfeed/elfeed.org"))
(elfeed-org)

(use-package "skewer-mode"
:config '(progn (add-hook 'js2-mode-hook 'skewer-mode)
          (add-hook 'css-mode-hook 'skewer-css-mode)
          (add-hook 'html-mode-hook 'skewer-html-mode)))

(use-package "monky")

(defun tkb-dont-do-that ()
  (interactive)
  (message "Don't DO that!  It hurts!")
  (beep))

(with-eval-after-load "simple"
  (define-key visual-line-mode-map (kbd "M-q") 'tkb-dont-do-that)
  (message "Defined M-q in visual-line-mode-map"))

(defun tkb-edit-filename-at-point ()
  (interactive)
  (find-file (thing-at-point 'filename t)))
(tkb-keys ((kbd "C-c F e") 'tkb-edit-filename-at-point))

(defun tkb-physical-screen-size ()
  (interactive)
  (destructuring-bind (_ width height)
      (assoc 'mm-size (frame-monitor-attributes))
    (let ((in-width (/ width 25.4))
          (in-height (/ height 25.4)))
      (message "%d×%dmm (%d×%din)" width height in-width in-height))))



;; If you are using Wayland, install the wl-clipboard package.  If you
;; are using X, install the xclip page.


(message "About to use-package org-download")
(use-package org-download
    :after org
    :defer nil
    :custom
    (org-download-method 'directory)
    (org-download-image-dir "Images")
    (org-download-heading-lvl nil)
    (org-download-timestamp "%Y%m%d-%H%M%S_")
    (org-image-actual-width 300)
    :bind
    ;; Why didn't this get bound?
    ("C-c k o d" . org-download-screenshot)
    :config
    ;; Why didn't org-download-clipboard work?
    (require 'org-download)
    (cond ((getenv "WAYLAND_DISPLAY")
            ;; check for Wayland first because of X on Wayland
            (message "Wayland is here!")
            (setq org-download-screenshot-method
                  "wl-paste > %s"))
          ((getenv "DISPLAY")
           (message "X is here!")
           (setq org-download-screenshot-method
            "xclip -selection clipboard -t image/png -o > %s"))
          (t
           (message "Neither X nor Wayland are available."))))
(message "After use-package org-download")

(defun tkb-open-file-at-point ()
  (interactive)
  (let ((filename (thing-at-point 'filename t)))
    (if filename
        (find-file filename)
      (message "No filename found at point"))))

;; Unicode subscripts
;; (apply #'string (cl-loop for i from 0 to 9 collect (+ #x2080 i)))
;;=> "₀₁₂₃₄₅₆₇₈₉"
(defun tkb-digit-to-unicode-subscript (d)
  "Convert a number between 0 and 9 into its Unicode subscript equivalent."
  (unless (<= 0 d 9) (error "%d isn't a number between 0 and 9, can't convert it to a subscript" d))
  (+ d #x2080))
;; Unicode superscripts are more complicated, because of hysterical raisons.
;;(apply #'string (cl-loop for i from 0 to 9 collect (if (<= 2 i 3) (+ #x00B0) (+ #x2070 i))))
;;=> "⁰ⁱ°°⁴⁵⁶⁷⁸⁹"
(defun tkb-digit-to-unicode-superscript (d)
  "Convert a number between 0 and 9 into its Unicode superscript equivalent."
  (unless (<= 0 d 9) (error "%d isn't a number between 0 and 9, can't convert it to a superscript" d))
  (if (= d 1) #x00B9
    (if (<= 2 d 3)
        (+ d #x00B0)
      (+ d #x2070))))
  
(defun tkb-unicode-fraction-the-hard-way (numerator denominator)
  "Return a string with a common fraction using unciode super- and sub-scripts."
  (interactive "nNumerator? \nnDenominator? ")
  (let ((numers (number-to-string (truncate (abs numerator))))
        (denoms (number-to-string (truncate (abs denominator)))))
    (concat (apply #'string (cl-loop for c across numers
                                  collect (tkb-digit-to-unicode-superscript (- c ?0))))
            "/"
            (apply #'string (cl-loop for c across denoms
                                  collect (tkb-digit-to-unicode-subscript (- c ?0)))))))
;; (tkb-unicode-fraction-the-hard-way 15 16)
;; (tkb-unicode-fraction-the-hard-way 1234567890 1234567890)

(defun tkb-insert-unicode-fraction-the-hard-way (numerator denominator)
  "Insert a common fraction using unicode super- and sub-scripts."
  (interactive "nNumerator? \nnDenominator? ")
  (insert (tkb-unicode-fraction-the-hard-way numerator denominator)))

(defun tkb-display-monitor-geometry ()
  (interactive)
  (cl-destructuring-bind (x y width height) (frame-monitor-geometry)
    (cl-destructuring-bind (mm-wide mm-high) (frame-monitor-attribute 'mm-size)
      (message "Monitor geometry is %d×%d@(%d,%d) on %d*%dmm" width height x y
               mm-wide mm-high))))

;;; ¿¿¿Write a function using display-monitor-attributes-list and probably
;;; pcase-dolist to display information on all attached monitor.???

(setq column-number-indicator-zero-based nil)

(message "End of tkb-experimental.el")
;;; end of tkb-experimental.el
