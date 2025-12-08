(defun new-music ()
  "List music artists that haven't been listened to yet.  See
~/Repos/tkb-notes/Music/listening.org."
  (interactive)
  (occur "^\\*+[ \t]*[A-Za-z:]"))
