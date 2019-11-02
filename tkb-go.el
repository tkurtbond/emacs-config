(use-package
 go-mode :ensure t
 :config
 (use-package go-guru :ensure t)

 ;; (go-guru-hl-identifier-mode) ; Enable highlighting.

 ;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
 (setq gofmt-command "goimports")

 (add-hook 'before-save-hook 'gofmt-before-save)

 ;; Running M-x golint will run golint on the current file. For more
 ;; usage, see Compilation-Mode:
 ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html

 (eval-after-load 'go-mode
   '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

 (add-hook 'go-mode-hook
	   '(lambda ()
	      (flycheck-mode)
	      (local-set-key (kbd "M-.") 'godef-jump)
	      (local-set-key (kbd "C-c C-k") 'godoc)
	      (local-set-key (kbd "C-c C-g") 'go-goto-imports)
	      (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports))))
