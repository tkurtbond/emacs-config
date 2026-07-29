;;;; .emacs.el - Unified emacs startup.

(setq debug-on-error nil)

;;(desktop-save-mode 1)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(when (< emacs-major-version 27)
  (package-initialize))

;; See (info "(emacs)Init File")
;;(setq inhibit-default-init t) ; C-h S is your friend

(require 'cl-lib)

(setq inhibit-startup-screen t)
(setq message-log-max 10000)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default indent-tabs-mode nil)
(setq backup-by-copying t)
;; ??? Revisit decision after a week, and after a month.
;; Go back to using double spaces, because I like the extra space and it
;; works so much better with groff.
;;(setq sentence-end-double-space nil) 

(setq line-move-visual nil)

(unless (version< emacs-version "29.1")
  ;; Error:
  ;; Failed to verify signature archive-contents.sig:
  ;; No public key for 066DAFCB81E42C40 created at 2019-10-29T17:10:02-0400 using RSA
  ;; Resolved by: https://www.reddit.com/r/emacs/comments/aug9in/failed_to_verify_signature_archivecontentssig/
  (setq package-check-signature nil)
  ;; Error:
  ;; Debugger entered--Lisp error: (file-error "https://elpa.gnu.org/packages/archive-contents" "Bad Request")
  ;; signal(file-error ("https://elpa.gnu.org/packages/archive-contents" "Bad Request"))  
  ;; Resolved by: https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  (require 'package)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
  ;; (package-initialize) ;; done in '~/.emacs'.

  (progn
    ;; I used to use ada-mode, gpr-mode, and gpr-query.
    (use-package ada-ref-man :ensure t)
    ;; Not using adoc-mode because of markup minimization making markup unusable.
    ;; a68-mode Does something weird with the list that maps file extensions to modes.
    (use-package auctex :ensure t)
    (use-package caml :ensure t)
    ;; I used to use cask, cask-mode, and caskxy
    (use-package cider :ensure t)
    (use-package clojure-mode :ensure t)
    (use-package clojure-quick-repls :ensure t)
    (use-package clojure-snippets :ensure t)
    (use-package cobol-mode :ensure t)
    (use-package dante :ensure t)       ; For Hasekll
    (use-package dash :ensure t)
    (use-package define-word :ensure t)
    (use-package disable-mouse :ensure t)
    (use-package docbook :ensure t)
    (use-package elfeed :ensure t)
    (use-package elfeed-org :ensure t)
    (use-package elpher :ensure t)
    ;; Did I ever really use elscreen?
    (use-package f :ensure t)
    (use-package fuel :ensure t)
    (use-package ac-geiser :ensure t)
    (use-package geiser :ensure t)
    (use-package geiser-chez :ensure t)
    (use-package geiser-chibi :ensure t)
    (use-package geiser-chicken :ensure t)
    (use-package geiser-guile :ensure t)
    (use-package geiser-racket :ensure t)
    (use-package gemini-mode :ensure t)
    (use-package haskell-mode :ensure t)
    (use-package js-comint :ensure t)
    (use-package js2-mode :ensure t)
    (use-package julia-mode :ensure t)
    (use-package lua-mode :ensure t)
    (use-package lsp-mode :ensure t)
    (use-package magit :ensure t)
    (use-package markdown-mode :ensure t)
    (use-package mew :ensure t)         ; Hope springs eternal
    (use-package monky :ensure t)
    (use-package nodejs-repl :ensure t)
    ;; Did I ever really use moz?
    ;; I used to use nim-mode, but something about unbalanced parentheses?
    (use-package nushell-mode :ensure t)
    (use-package oberon :ensure t)
    (use-package org-download :ensure t)
    (use-package origami :ensure t)
    (use-package php-mode :ensure t)
    (use-package projectile :ensure t)
    (use-package racket-mode :ensure t)
    (use-package rec-mode :ensure t)
    ;; I used to use realgud and realgud-lldb on macOS, I think?
    (use-package s :ensure t)
    (use-package shadchen :ensure t)
    (use-package skewer-mode :ensure t)
    (use-package slime :ensure t)
    (use-package string-inflection :ensure t)
    (use-package unfill :ensure t)
    (use-package unicode-fonts :ensure t)
    (use-package visual-fill-column :ensure t)
    (use-package wanderlust :ensure t)  ; Apparently using again.
    (use-package w3m :ensure t)
    (use-package web-mode :ensure t)
    (use-package yaml-mode :ensure t)
    (use-package ada-ts-mode :ensure t)
    (use-package gpr-ts-mode :ensure t)
    (use-package gpr-yasnippets :ensure t)
    (use-package company :ensure t)
    )

  (load "s")                      ; Because its autoloads didn't work.
  (load "f")                      ; Ditto.
  )

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

(progn
  ;; http://emacsredux.com/blog/2013/04/01/highlight-matching-parentheses/
  (require 'paren)
  (setq show-paren-style 'parenthesis)
  (show-paren-mode +1))

(when nil 
  ;; http://emacsredux.com/blog/2013/04/02/highlight-current-line/
  (global-hl-line-mode +1))


(setq custom-file "~/lib/emacs/emacs-config/tkb-custom.el")
(load custom-file)

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-line-limit 20)

(load (cl-case system-type
	((ms-dos windows-nt)
	 "~/lib/emacs/emacs-config/mswoe-init.el")
	((vax-vms axp-vms)
	 "~/lib/emacs/emacs-config/vms-init.el")
	(t
	 "~/lib/emacs/emacs-config/unix-init.el")))

(when (eq 'darwin system-type)
  (load "~/lib/emacs/emacs-config/macos-init.el"))

(load "~/lib/emacs/emacs-config/tkb-gnus.el")

;(load "~/lib/emacs/emacs-config/tkb-mh-e.el")

;; For some reason emacs-w3m ends up under
;; c:/emacs/emacs-VER/share/emacs/site-lisp
(let* ((emacs-dir (file-name-directory (directory-file-name data-directory)))
       (share-site-lisp-dir (concat emacs-dir "share/emacs/site-lisp/")))
  (when-directory (dir share-site-lisp-dir)
    ;; I looked at startup.el normal-top-level, and this is what they do.
    (let ((default-directory dir))
      (load (expand-file-name "subdirs.el") t t t))))

(when-directory (d (expand-file-name "~/local/share/emacs/site-lisp/"))
  (add-to-list 'load-path d)
  (let ((default-directory d))
    (load (expand-file-name "subdirs.el") t t t)))

(when-directory (d "~/local/share/info/")
  (add-to-list 'Info-default-directory-list d))

(when nil
  ;; 2012-09-15: this doesn't work on arch, where wl/ ends up under
  ;; /usr/share/emacs/site-lisp but wl/icons/ ends up under
  ;; /usr/share/emacs/24.2/etc/.
  (when-load-dir (d "wl")
    (when (string-match "^\\(.*\\)/site-lisp/wl" d)
      (setq wl-icon-directory (concat (match-string 1 d) "/etc/wl/icons/"))
      (unless (file-directory-p wl-icon-directory)
	(error "%s: wl-icon-directory is wrong: %S " #$ wl-icon-directory)))
    d))

(when nil 
  (when (locate-file "w3m" load-path '(".el" ".el.gz"))
    (load "~/lib/emacs/emacs-config/tkb-w3m.el")))
(load "~/lib/emacs/emacs-config/tkb-fortune.el")
(when nil (load "~/lib/emacs/emacs-config/tkb-timeclock.el"))

(when window-system
  (add-hook 'after-init-hook
	    (lambda ()
              (load "~/lib/emacs/emacs-config/tkb-gui-setup")
              (load "~/lib/emacs/emacs-config/tkb-gui-fixup"))))

;; wanderlust
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))



(put 'set-goal-column 'disabled nil)


(load-file "~/lib/emacs/emacs-config/tkb-time-expansion.el")
(load-file "~/lib/emacs/emacs-config/tkb-time-expansion-keys.el")
(load-file "~/lib/emacs/emacs-config/tkb-status-reports.el")
(load-file "~/lib/emacs/emacs-config/tkb-blog.el")
(when-directory (d (expand-file-name "~/Repos/microblog/emacs/"))
  (load-file "~/Repos/microblog/emacs/tkb-microblog.el"))
(load-library "gemini-mode.el")
(load-file "~/lib/emacs/emacs-config/tkb-magit.el")
(load-file "~/lib/emacs/emacs-config/tkb-org.el")
(load-file "~/lib/emacs/emacs-config/tkb-music.el")

;;(setq epg-gpg-program "gpg2") ; on macOS with homebrew it's gpg.

(when nil
  (add-hook 'find-file-hooks
	  (lambda ()
	    (when buffer-read-only
	      (set-background-color "yellow")))))

(defun t:bfn ()
  "Current buffer's filename without directory."
  (file-name-nondirectory (buffer-file-name)))

(defun t:bfnse ()
  "Current buffer's filename without directory and without extension."
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

(when (and (eq system-type 'darwin)
	   (eq window-system 'ns))
  ;; Should this go in macosx-init.el?
  ;; #5683 - 23.1.93; list-colors-display doesn't show all colors - GNU bug report logs - http://debbugs.gnu.org/cgi/bugreport.cgi?bug=5683#11
  (setq x-colors (ns-list-colors)))

(defun t:make-executable (filename)
  "Make the file belonging to the current buffer executable."
  (interactive (list (buffer-file-name)))
  (let* ((old-modes (file-modes filename))
	 (new-modes (file-modes-symbolic-to-number "u+x" old-modes)))
    (message "Converting %s from %o to %o" filename old-modes new-modes)
    (chmod filename new-modes)))

(require 'unicode-fonts)
(unicode-fonts-setup)

;;(require 'mercurial)

(setq abbrev-file-name "~/lib/emacs/emacs-config/abbrev_defs")

(setq auth-sources '((:source "~/.authinfo.gpg")))

(load "~/lib/emacs/emacs-config/tkb-last.el")
;; end of init.el
