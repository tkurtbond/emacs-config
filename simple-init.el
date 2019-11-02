
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq tkb-default-font 
      "-*-Go Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
;;(set-frame-font tkb-default-font)
;;(set-frame-height nil 50)
;;(set-frame-width nil 80)
(setq tkb-default-frame-alist
      `((width . 80)
	(height . 50)
	(top . 10)
	(left . 10)
	(font . ,tkb-default-font)
	(background-color . "wheat")
	(foreground-color . "black")
	(cursor-color . "orange")))

(setq default-frame-alist
      (append default-frame-alist tkb-default-frame-alist))
(setq initial-frame-alist default-frame-alist)
(tool-bar-mode -1)
(set-frame-parameter nil 'font tkb-default-font)
(set-frame-parameter nil 'top 10)
(set-frame-parameter nil 'left 10)

(defun tkb-insert-iso-date ()
  (interactive)
  (insert (format-time-string "%Y-%2m-%2d")))

(global-set-key "\C-cdi" #'tkb-insert-iso-date)
(global-set-key "\C-cC" #'compile)
(setq require-final-newline 'ask)
(setq completion-ignore-case t)

;;(require 'yaml-mode)
;;(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (yaml-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
