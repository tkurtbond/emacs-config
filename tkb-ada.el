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

(defun tkb-ada-filename-to-dashes (filename)
  (let* ((sans-extension
          (if (string-match "\\([.]+\\)\\(.ads\\|.adb\\)\\'" filename)
              (match-string 1 filename)
            filename))
         (with-dashes (cl-loop
                            with characters = '()
                            for e across sans-extension
                            collect (if (= e ?.) ?- e) into characters
                            finally return (apply #'string characters))))
    (if (string-equal sans-extension filename)
        with-dashes
      (concat with-dashes (file-name-extension filename)))))

(defun tkb-find-adaincludes-file (filename)
  "Find an Ada file along the TKB_ADAINCLUDES_PATH."
  (interactive "sAda Includes Filename: ")
  (let* (;; First, add .ads if the filename has no extension.
         (filename (if (string-match-p "\\(.*\\)\\(ads\\|adb\\)\\'" filename)
                       filename
                     (concat filename ".ads")))
         ;; Search with the original name.  Note that pkg.subpkg.ads
         ;; is a valid name to gnat, although it will give a warning about
         ;; the name not matching.
         (result-filename (tkb-search-path-list tkb-adaincludes-path filename))
         ;; If it was not found, try the filename with dashes.
         (result-filename
          (if result-filename
              result-filename
            (tkb-search-path-list tkb-adaincludes-path
                                  (tkb-ada-filename-to-dashes filename))))
         ;; If it was not found, try with the krunched filename.
         (result-filename
          (if result-filename
              result-filename
            (tkb-search-path-list tkb-adaincludes-path
                                  (tkb-krunch-ada-filenname
                                   filename)))))
  
  result-filename))

(defun x ()
  (interactive)
  (skip-chars-backward ".[:alnum:]_-")
  (if (looking-at "\\([.[:alnum:]_-]+\\):\\([0-9]+\\)")
      (message "%s:%s" (match-string 1) (match-string 2)))
  )

(defun tkb-krunch-ada-filenname (filename)
  "Use gnatkr to krunch an Ada source filename into short form.  If the
filename does not have the extension \".ads\" or \".adb\" then \".ads\"
is added."
  (interactive "MFile name? ")
  (let* ((filename (if (string-match-p "ads\\|adb"
                                       (file-name-extension filename))
                       filename
                     (concat filename ".ads")))
         (buf (generate-new-buffer "*tkb-gnatkr*"))
         (_ (call-process "gnatkr" nil buf nil filename))
         (krunched-name (s-trim-right
                         (with-current-buffer buf
                           (buffer-substring-no-properties (point-min)
                                                           (point-max))))))
    (when (called-interactively-p) (message "%s" krunched-name))
    krunched-name))

(defun tkb-get-ada-spec-file ()
  "Find a Ada package name at point and get the krunched name of its
source file."
  (interactive)
  (let* ((id (substring-no-properties (thing-at-point 'tkb-ada-identifier)))
         (filename (concat id ".ads"))
         (krunched-name (tkb-krunch-ada-filenname filename)))
    (when (called-interactively-p)
      (message "file name: %s" krunched-name))
    krunched-name))

(defun tkb-find-ada-file (filename)
  "Find an Ada filename in the path TKB_ADAINCLUDES_PATH and display it."
  (interactive "sFilename? ")
  (let ((result-filename (tkb-find-adaincludes-file filename)))
    (cond (result-filename
           (find-file-read-only result-filename))
          (t
           (beep)
           (message "Ada file %s not found" filename)))))

(defun tkb-find-ada-file-other-window (filename)
  "Find an Ada filename in the path TKB_ADAINCLUDES_PATH and display it in
another window."
  (interactive "sFilename? ")
  (let* ((result-filename (tkb-find-adaincludes-file filename)))
    (cond (result-filename
           (find-file-read-only-other-window result-filename))
          (t
           (beep)
           (message "Ada file %s not found" filename)))))

(defun tkb-find-ada-spec-file ()
  "Find the Ada file specified by the Ada package at point and display it."
  (interactive)
  (tkb-find-ada-file (tkb-get-ada-spec-file)))

(defun tkb-find-ada-spec-file-other-window ()
  "Find the Ada file specified by the Ada package at point and display it
in another window."
  (interactive)
  (tkb-find-ada-file-other-window (tkb-get-ada-spec-file)))

(define-key tkb-ada-map "s" #'tkb-find-ada-spec-file)
(define-key tkb-ada-map "4s" #'tkb-find-ada-spec-file-other-window)
(define-key tkb-ada-map "f" #'tkb-find-ada-file)
(define-key tkb-ada-map "4f" #'tkb-find-ada-file-other-window)
