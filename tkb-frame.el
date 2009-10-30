
(defun list-frames ()
  (interactive)
  (let ((buf (get-buffer-create " *Frame List*")))
    (save-excursion
      (set-buffer buf)
      (delete-region (point-min) (point-max))
      (loop for frame in (frame-list)
	    do (progn
		 (insert (format "%-20s %-20s %dx%d\n"
				 (frame-parameter frame 'name)
				 (or (frame-parameter frame 'tkb-frame-name)
				     "")				   
				 (frame-width frame)
				 (frame-height frame)))
		 (loop for window in (window-list frame)
		       do (progn
			    (insert (format "> %s\n" (buffer-name (window-buffer window)))))))
	    (display-buffer buf 'not-this-window)))))


(defun tkb-after-make-frame (frame)
  (let (fname (read-from-minibuffer "TKB Frame Name: "))
    (unless (empty-string-p fname)
      (setf (frame-parameter frame 'tkb-frame-name) fname))))

(add-hook 'after-make-frame-functions #'tkb-after-make-frame)
