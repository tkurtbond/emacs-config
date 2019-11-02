;; See Info: (wl.info)BBDB.


(require 'bbdb-wl)

(bbdb-wl-setup)
;; enable pop-ups
(setq bbdb-use-pop-up t)
;; auto collection
(setq bbdb/mail-auto-create-p t)
;; exceptional folders against auto collection
(setq bbdb-wl-ignore-folder-regexp "^@")
(setq signature-use-bbdb t)
(setq bbdb-north-american-phone-numbers-p t)
;; shows the name of bbdb in the summary :-)
(setq wl-summary-from-function 'bbdb-wl-from-func)
;; automatically add mailing list fields
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))))
