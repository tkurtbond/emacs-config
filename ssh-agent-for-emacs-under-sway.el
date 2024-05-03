;; Example used:
(setenv "SSH_AUTH_SOCK" (expand-file-name "~/.ssh/agent"))
;; XDG/Fedora compatable?
(setenv "SSH_AUTH_SOCK" (s-concat (getenv "XDG_RUNTIME_DIR") "/ssh-agent.socket"))
