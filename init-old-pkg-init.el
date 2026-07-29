(let ((tkb-packages '(
		      ;; installing ada-mode using package-install
		      ;; says its already installed because
		      ;; ada-mode.el comes with emacs; so installing
		      ;; a newer version only works from the
		      ;; list-packages buffer.
		      ;; ada-mode
                      ;; gpr-mode
                      ;; gpr-query
		      ada-ref-man
		      ;; Not using because of markup minimization making
		      ;; markup unusable.
		      ;;adoc-mode
                      ;; Does something weird with the list that maps file extensions to modes.
                      ;;a68-mode
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
                      lua-mode
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
                      unfill
                      unicode-fonts
		      use-package ;; too strict?
                      visual-fill-column
		      wanderlust ;; apparently using again.
                      w3m
                      web-mode
                      yaml-mode

                      ada-ts-mode
                      gpr-ts-mode
		      gpr-yasnippets
		      company
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
