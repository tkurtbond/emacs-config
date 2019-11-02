(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(add-hook 'org-mode-hook 'turn-on-font-lock)  ; org-mode buffers only

(setq org-todo-keywords '((sequence "TODO" "ONGOING" "DONE")))
