;;-*-coding: utf-8;-*-
(define-abbrev-table 'basic-mode-abbrev-table
  '(
    ("call putstr(codef, level," "cps" nil :count 0)
    ("cpl" "call putline(codef)" nil :count 24)
    ("cps" "call putstr(codef, level," nil :count 20)
    ("func" "function ()
end function" nil :count 1)
    ("opts" "option type = explicit, constant type = integer, &
        size = integer long, size = real double" nil :count 4)
    ("proc" "sub ()
end sub" nil :count 1)
    ("prog" "program
end program" nil :count 1)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("bpu" "begin_pasteboard_update" nil :count 5)
    ("gv" "get_value" nil :count 44)
    ("rcp" "return_cursor_pos" nil :count 4)
    ("sda" "scroll_display_area" nil :count 6)
    ("sdsr" "set_display_scrolling_region" nil :count 7)
    ("ss" "set_symbol" nil :count 0)
    ("sss" "stat = set_symbol (s, tbuf, symbol_table);" nil :count 52)
    ("st" "symbol_table" nil :count 2)
   ))

(define-abbrev-table 'oberon-mode-abbrev-table
  '(
    ("array" "ARRAY" indent-according-to-mode :count 0)
    ("begin" "BEGIN" indent-according-to-mode :count 0)
    ("boolean" "BOOLEAN" indent-according-to-mode :count 0)
    ("by" "BY" indent-according-to-mode :count 0)
    ("case" "CASE" indent-according-to-mode :count 0)
    ("char" "CHAR" indent-according-to-mode :count 0)
    ("const" "CONST" indent-according-to-mode :count 0)
    ("div" "DIV" indent-according-to-mode :count 0)
    ("do" "DO" indent-according-to-mode :count 0)
    ("else" "ELSE" indent-according-to-mode :count 0)
    ("elsif" "ELSIF" indent-according-to-mode :count 0)
    ("end" "END" indent-according-to-mode :count 0)
    ("exit" "EXIT" indent-according-to-mode :count 0)
    ("false" "FALSE" indent-according-to-mode :count 0)
    ("for" "FOR" indent-according-to-mode :count 0)
    ("if" "IF" indent-according-to-mode :count 0)
    ("import" "IMPORT" indent-according-to-mode :count 0)
    ("in" "IN" indent-according-to-mode :count 0)
    ("integer" "INTEGER" indent-according-to-mode :count 0)
    ("is" "IS" indent-according-to-mode :count 0)
    ("longint" "LONGINT" indent-according-to-mode :count 0)
    ("longreal" "LONGREAL" indent-according-to-mode :count 0)
    ("loop" "LOOP" indent-according-to-mode :count 0)
    ("mod" "MOD" indent-according-to-mode :count 0)
    ("module" "MODULE" indent-according-to-mode :count 0)
    ("nil" "NIL" indent-according-to-mode :count 0)
    ("of" "OF" indent-according-to-mode :count 0)
    ("or" "OR" indent-according-to-mode :count 0)
    ("pointer" "POINTER" indent-according-to-mode :count 0)
    ("procedure" "PROCEDURE" indent-according-to-mode :count 0)
    ("real" "REAL" indent-according-to-mode :count 0)
    ("record" "RECORD" indent-according-to-mode :count 0)
    ("repeat" "REPEAT" indent-according-to-mode :count 0)
    ("return" "RETURN" indent-according-to-mode :count 0)
    ("set" "SET" indent-according-to-mode :count 0)
    ("shortint" "SHORTINT" indent-according-to-mode :count 0)
    ("then" "THEN" indent-according-to-mode :count 0)
    ("to" "TO" indent-according-to-mode :count 0)
    ("true" "TRUE" indent-according-to-mode :count 0)
    ("type" "TYPE" indent-according-to-mode :count 0)
    ("until" "UNTIL" indent-according-to-mode :count 0)
    ("var" "VAR" indent-according-to-mode :count 0)
    ("while" "WHILE" indent-according-to-mode :count 0)
    ("with" "WITH" indent-according-to-mode :count 0)
   ))

(define-abbrev-table 'sdcl-mode-abbrev-table
  '(
    ("wse" "write sys$error" nil :count 5)
    ("wso" "write sys$output" nil :count 6)
   ))

(define-abbrev-table 'sql-mode-abbrev-table
  '(
    ("cr" ("create") nil :count 0)
    ("del" ("delete") nil :count 0)
    ("func" ("function") nil :count 0)
    ("ins" ("insert") nil :count 0)
    ("proc" ("procedure") nil :count 0)
    ("sel" ("select") nil :count 0)
    ("upd" ("update") nil :count 0)
   ))

