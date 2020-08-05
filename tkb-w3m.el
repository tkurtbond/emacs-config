(setq browse-url-browser-function 'w3m)
(progn 
  ;; See http://cooking-with-lisp.blogspot.com/2005/07/w3m-customization.html
  ;; for good customizations
  (progn
    (eval-after-load "w3m"
      '(defadvice vm-mouse-send-url (around tkb-vm-mouse-send-url activate)
	 (apply #'browse-url (cons url switches)))))

  (when-load-file "w3m"
    (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
    (eval-after-load "w3m"
      '(progn
	 (define-key w3m-mode-map "T" 'w3m-view-this-url-new-session)
	 (tkb-keys ((kbd "C-c k G") 'w3m-goto-url-new-session)
		   ((kbd "C-c k g") 'w3m-goto-url)))))

  (when-load-file "w3m-load"
    (require 'w3m-load)
    (setq w3m-view-this-url-new-session-in-background t)
    (setq w3m-make-new-session t)
    (setq browse-url-new-window-flag t)))

;; For MH-E problems with w3m.
(setq w3m-imitate-widget-button t)
(tkb-keys
  ((kbd "C-c k b") #'browse-url-at-point)
  ((kbd "C-c k w") #'widget-forward)
  ([f5] #'browse-url)
  ([f6] #'browse-url-at-point)
  ([f12] #'widget-forward))
