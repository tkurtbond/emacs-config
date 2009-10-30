;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tkb-www.el -- World-wide-web
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $ProjectHeader: tkbconfig 0.5 Tue, 09 May 2000 20:50:39 -0400 tkb $
;;; $Id: tkb-www.el 1.1 Sun, 26 Mar 2000 15:10:50 -0500 tkb $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq browse-url-browser-function (function browse-url-mmm))
(setq browse-url-new-window-p t)

(when nil
  (cond (window-system
	 (setq browse-url-browser-function 'browse-url-netscape)
	 (setq browse-url-netscape-program "firefox"))
	(t
	 (setq browse-url-browser-function
	       ;; 'browse-url-w3
	       'w3m-browse-url))))


;(setq browse-url-browser-function 'tkb-browse-url-netscape)

(defun tkb-browse-url-netscape (url &optional new-win-p)
  (let ((new-window-p (or new-win-p
			  browse-url-new-window-p)))
    (browse-url-netscape url new-window-p)))

(defun mmm:send-url-to-mmm (url)
  (message "Sending URL to MMM...")
  (save-excursion
    (set-buffer (get-buffer-create "*Shell Command Output*"))
    (erase-buffer)
    ;; don't worry about this failing...
    (call-process "mmm_remote" nil 0 nil url)
    (message "Sending URL to MMM... done")))

;;; end of tkb-www.el
