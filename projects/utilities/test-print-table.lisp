(in-package :utilities)

(prologue test-print-table)

(test row-extend (row-extend '((1 2 3) (1)) 5) '((1 2 3 "" "") (1 "" "" "" "")))

(test row-extend-w-subtable
 (row-extend '((1 2 3) (:subtable (4 5))) 5) '((1 2 3 "" "") (:subtable (4 5))))

(defmacro test-print-table (name rows expected-output)
  `(test ,name
    (with-output-to-string (stream)
      (print-table ',rows :stream stream))
    ,(substitute #\newline #\_ expected-output)))

(test-print-table 
 print-table
 ((1 2 3))
 "1  2  3  _")

(test-print-table 
 print-table-unevent-lengths
 ((1 2 3) (4 5) (6))
"1  2  3  _4  5     _6        _")

(test-print-table
 print-table-subpanel
 ((1 2 3) (:subtable (4 5 6)))
 "1  2  3  _  4  5  6  _")
