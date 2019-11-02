(flet ((fts (&rest args) (apply #'format-time-string args)))
  (let* ((today         (current-time))
         (iso-year      (format-time-string "%G" today))
         (n-iso-year    (string-to-number iso-year))
         (iso-week      (format-time-string "%V" today))
         (n-iso-week    (string-to-number iso-week))
         (iso-dow       (format-time-string "%u" today))
         (n-iso-dow     (string-to-number iso-dow))
         (start-delta   (days-to-time (- n-iso-dow)))
         (week-start    (time-subtract today start-delta))
         (week-end      (time-add week-start (days-to-time 6)))
         (week-dir      (format "%s/W%s" iso-year iso-week))
         (day-filename  (format "'%s-%s.rst' "
				iso-dow (fts "%A-%Y-%m-%d" today)))
         (dates-covered (concat (fts "%Y-%m-%d" week-start) " to "
				(fts "%Y-%m-%d" week-end)))
         (fmt-pretty    "%A, %e %B %Y")
         (dates-pretty  (concat (fts fmt-pretty week-start) " to "
				(fts fmt-pretty week-end))))
    (message "%S" today)
    (message "%s" (concat (fts "%Y-%m-%d" today) "\n"
                          week-dir "\n"
                          day-filename "\n"
                          dates-covered "\n"
                          dates-pretty))))
