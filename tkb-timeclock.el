(require 'timeclock)

(defun tkb-timeclock-change (&optional arg project)
  "Change to working on a different project.
This clocks out of the current project, then clocks in on a new one.
With a prefix ARG, consider the previous project as finished at the
time of changeover.  PROJECT is the name of the last project you were
working on."
  (interactive "P")
  (timeclock-out arg nil t)
  (timeclock-in nil project (interactive-p)))
(setq timeclock-ask-before-exiting t)
(timeclock-modeline-display)

(define-key ctl-x-map "ti" 'timeclock-in)
(define-key ctl-x-map "to" 'timeclock-out)
(define-key ctl-x-map "tc" 'tkb-timeclock-change)
(define-key ctl-x-map "tr" 'timeclock-reread-log)
(define-key ctl-x-map "tu" 'timeclock-update-modeline)
(define-key ctl-x-map "tv" 'timeclock-visit-timelog)
(define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)


