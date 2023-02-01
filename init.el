;;;; .emacs.el - Unified emacs startup.

;; See (info "(emacs)Init File")
;;(setq inhibit-default-init t) ; C-h S is your friend

(require 'cl-lib)

(setq inhibit-startup-screen t)
(setq message-log-max 10000)
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default indent-tabs-mode nil)

(setq line-move-visual nil)

(unless (version< emacs-version "24.1.1")
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
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;; (package-initialize) ;; done in '~/.emacs'.

  (let ((tkb-packages '(
			;; installing ada-mode using package-install
			;; says its already installed because
			;; ada-mode.el comes with emacs; so installing
			;; a newer version only works from the
			;; list-packages buffer.
			;; ada-mode ; not currently using
			ada-ref-man ; not currently using
			;; Not using because of markup minimization making
			;; markup unusable.
			;;adoc-mode
			auctex
			caml
                        ;; cask
                        ;; cask-mode
                        ;; caskxy
			cider
			clojure-mode
			clojure-quick-repls
			clojure-snippets
                        dante           ; For Haskell
                        dash
                        define-word
			docbook
			;;elscreen ; Did I ever really use this?
                        elpher
			f
			fuel
			ac-geiser geiser geiser-chez geiser-chibi geiser-chicken
                        geiser-guile geiser-racket
                        haskell-mode
                        js-comint
                        julia-mode
                        magit
			markdown-mode
			;; moz ; Did I every really use this?
			nim-mode
                        nodejs-repl
                        oberon
			projectile 
			racket-mode
                        ;;+++
                        ;; These are available both from gnu and melpa,
                        ;; so install manually.
                        ;; realgud
                        ;; realgud-lldb
                        ;;---
			;; regex-tool ; not currently using
                        s
                        string-inflection
                        unicode-fonts
			use-package ;; too strict?
                        visual-fill-column
			wanderlust ;; apparently using again.
                        w3m
                        yaml-mode
			)))
    (unless (cl-every #'package-installed-p tkb-packages)
      (package-refresh-contents))
    (dolist (p tkb-packages)
      (unless (package-installed-p p)
	(package-install p)))
    (load "s.el")                  ; Because its autoloads didn't work.
    (load "f.el")                  ; Ditto.
    ))

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


(setq custom-file "~/lib/emacs/tkb/tkb-custom.el")
(load custom-file)

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-line-limit 20)

(load (cl-case system-type
	((ms-dos windows-nt)
	 "~/lib/emacs/tkb/mswoe-init.el")
	((vax-vms axp-vms)
	 "~/lib/emacs/tkb/vms-init.el")
	(t
	 "~/lib/emacs/tkb/unix-init.el")))

(when (eq 'darwin system-type)
  (load "~/lib/emacs/tkb/macos-init.el"))

(load "~/lib/emacs/tkb/tkb-gnus.el")

;(load "~/lib/emacs/tkb/tkb-mh-e.el")

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
    (load "~/lib/emacs/tkb/tkb-w3m.el")))
(load "~/lib/emacs/tkb/tkb-fortune.el")
(when nil (load "~/lib/emacs/tkb/tkb-timeclock.el"))

(when window-system
  (add-hook 'after-init-hook
	    (lambda () (load "~/lib/emacs/tkb/tkb-gui-setup"))))

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


(load-file "~/lib/emacs/tkb/tkb-time-expansion.el")
(load-file "~/lib/emacs/tkb/tkb-time-expansion-keys.el")
(load-file "~/lib/emacs/tkb/tkb-status-reports.el")
(load-file "~/lib/emacs/tkb/tkb-blog.el")
(load-file "~/lib/emacs/tkb/tkb-microblog.el")


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

(setq abbrev-file-name "~/lib/emacs/tkb/abbrev_defs")

(setq auth-sources '((:source "~/.authinfo.gpg")))

(load "~/lib/emacs/tkb/tkb-last.el")
;; end of init.el
