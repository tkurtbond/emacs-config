;;;; .emacs.el - Unified emacs startup.

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
			;; ada-mode
                        ;; gpr-mode
                        ;; gpr-query
                        ;; ada-ts-mode
			ada-ref-man
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
                        cobol-mode
                        dante           ; For Haskell
                        dash
                        define-word
                        disable-mouse
			docbook
                        elfeed
                        elfeed-org
                        elpher
			;;elscreen ; Did I ever really use this?
			f
                        ;; forth-mode
			fuel
			ac-geiser geiser geiser-chez geiser-chibi geiser-chicken
                        geiser-guile geiser-racket
                        gemini-mode
                        haskell-mode
                        js-comint
                        js2-mode
                        julia-mode
                        lsp-mode
                        magit
			markdown-mode
                        mew             ; Hope springs eternal
                        monky
			;; moz ; Did I every really use this?
			;;nim-mode ; unbalanced parentheses.
                        nodejs-repl
                        nushell-mode
                        oberon
                        org-download
                        origami
                        php-mode
			projectile 
			racket-mode
                        rec-mode
                        ;;+++
                        ;; These are available both from gnu and melpa,
                        ;; so install manually.
                        ;; realgud
                        ;; realgud-lldb
                        ;;---
			;; regex-tool ; not currently using
                        s
                        shadchen
                        skewer-mode
                        slime
                        string-inflection
                        unicode-fonts
			use-package ;; too strict?
                        visual-fill-column
			wanderlust ;; apparently using again.
                        w3m
                        web-mode
                        yaml-mode
			)))
    ;; The order of things here might be mistaken.  Should I iterate
    ;; over tkb-packages and then over package-selected-pages?
    (message "tkb's packages: %S" tkb-packages)
    (unless (cl-every #'package-installed-p package-selected-packages)
      (package-refresh-contents))
    (dolist (p tkb-packages) ;; was package-selected-packages
      (message "selected package %s" p)
      (unless (package-installed-p p)
        (message "installing package %s" p)
	(package-install p)))
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
