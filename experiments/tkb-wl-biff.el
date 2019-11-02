(setq wl-biff-check-folder-list '("%INBOX" "+inbox"))
(setq wl-biff-notify-hook '(ding))

;; Set mail-icon to be shown universally in the modeline.
(setq global-mode-string
      (cons
       '(wl-modeline-biff-status
         wl-modeline-biff-state-on
         wl-modeline-biff-state-off)
       global-mode-string))


