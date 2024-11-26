;; Unfinished stuff for Ada.
(defun tkb-search-path-list (path-list filename)
  (cl-loop for dirname in path-list
        for result-filename = (file-name-concat dirname filename)
        if (file-exists-p result-filename) return result-filename))

(defconst tkb-adaincludes-path
  (let ((p (getenv "TKB_ADAINCLUDES_PATH")))
    (if p
        (s-split ":" p)
      '("/usr/local/sw/src/gcc/gcc/ada/libgnat")))
  "List containing path elements to search for Ada spec files in.")


(require 'thingatpt)
(define-thing-chars tkb-ada-filename ".[:alnum:]_-")
(define-thing-chars tkb-ada-identifier "[:alnum:]_.")
(defun tkb-find-adainclude ()
  (interactive)
  (let* ((filename (thing-at-point 'tkb-ada-filename))
         (pathname (string-join (list tkb-adaincludes-directory
                                      filename))))
    (message "%s" pathname)))

(defun tkb-find-adaincludes-file (filename)
  "Find an Ada file along the TKB_ADAINCLUDES_PATH."
  (interactive "sAda Includes Filename: ")
  (tkb-search-path-list tkb-adaincludes-path filename))

(defun x ()
  (interactive)
  (skip-chars-backward ".[:alnum:]_-")
  (if (looking-at "\\([.[:alnum:]_-]+\\):\\([0-9]+\\)")
      (message "%s:%s" (match-string 1) (match-string 2)))
  )

(defun tkb-get-ada-spec-file ()
  (interactive)
  (let* ((id (substring-no-properties (thing-at-point 'tkb-ada-identifier)))
         (filename (concat id ".ads"))
         (buf (generate-new-buffer "*tkb-gnatkr*"))
         ;; krunched name
         (_ (call-process "gnatkr" nil buf nil filename))
         (krunched-name (s-trim-right
                         (with-current-buffer buf
                          (buffer-substring-no-properties (point-min)
                                                          (point-max))))))
    (message "file name: %s" krunched-name)
    krunched-name))


(defun tkb-find-ada-file (filename)
  (interactive "sFilename? ")
  (let ((result-filename (tkb-search-path-list tkb-adaincludes-path filename)))
    (cond (result-filename
           (find-file-read-only result-filename))
          (t
           (beep)
           (message "Ada file %s not found" filename)))))

(defun tkb-find-ada-file-other-window (filename)
  (interactive "sFilename? ")
  (let ((result-filename (tkb-search-path-list tkb-adaincludes-path filename)))
    (cond (result-filename
           (find-file-read-only-other-window result-filename))
          (t
           (beep)
           (message "Ada file %s not found" filename)))))

(defun tkb-find-ada-spec-file ()
  (interactive)
  (tkb-find-ada-file (tkb-get-ada-spec-file)))

(defun tkb-find-ada-spec-file-other-window ()
  (interactive)
  (tkb-find-ada-file-other-window (tkb-get-ada-spec-file)))

(define-key tkb-ada-map "s" #'tkb-find-ada-spec-file)
(define-key tkb-ada-map "4s" #'tkb-find-ada-spec-file-other-window)
