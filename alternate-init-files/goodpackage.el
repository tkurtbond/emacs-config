;;; goodpackage.el -- load with emacs -q -l ~/tkb/lib/emacs/tkb/alternate-init-files/goodpackage.el
;;; so that list-package will work right.

;; See ~/lib/emacs/tkb/init.el
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
