(defvar cpb-term-command "osascript -e '
on run argv
  tell application \"Terminal\"
    do script (\"cd \" & item 1 of argv) in window 1
  end tell
end run
' '%s'")


(defun cpb-term-here ()
  (interactive)
  (shell-command (format cpb-term-command default-directory)))

;; From emacs execute cpb-term-here and the current tab in the
;; terminal will pop to the same directory as the buffer.  Works with
;; dried buffers too.

;; alias em='emacsclient -n'
;; Then I can edit files (and directories) in my main emacs just by
;; typing em <file|dir name>
