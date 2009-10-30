(defun tkb-show-abbrev-settings ()
  (message "save-abbrevs: %S abbrevs-changed: %S\n"
	   save-abbrevs abbrevs-changed))

(defadvice define-abbrev (around tkb-dbg-define-abbrev activate)
  (message "tkb-dbg-define-abbrev table %S name %s\n" table name)
  (backtrace)
  ad-do-it)
(defadvice define-abbrev-table (around tkb-dbg-define-abbrev-table
				       activate)
  (message "tkb-dbg-define-abbrev-table table %s\n" tablename)
  (backtrace)
  ad-do-it)
