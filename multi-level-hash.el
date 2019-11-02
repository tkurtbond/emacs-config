(defun put-multi-hash (keylist hash-table value)
  "Store a value in a multi-level hash table: one level for each element of keylist"
  (let ((test (hash-table-test hash-table)))
    (if (= (length keylist) 1)
	(setf (gethash (first keylist) hash-table) value)
      (let ((table1 (or (gethash (first keylist) hash-table)
			(setf (gethash (first keylist) hash-table)
			      (make-hash-table :test test)))))
	(put-multi-hash (rest keylist) table1 value)))))
(defalias 'pmh 'put-multi-hash)

(defun get-multi-hash (keylist hash-table)
  "Fetch a value from a multi-level hash table: one level for each element of keylist"
  (let ((test (hash-table-test hash-table)))
    (if (= (length keylist) 1)
	(gethash (first keylist) hash-table)
      (let ((table1 (or (gethash (first keylist) hash-table)
			(setf (gethash (first keylist) hash-table)
			      (make-hash-table :test test)))))
	(get-multi-hash (rest keylist) table1)))))
(defalias 'gmh 'get-multi-hash)

(defsetf get-multi-hash put-multi-hash)

(when nil 
  (setf x (make-hash-table :test #'equal))
  (put-multi-hash '("one" "two" "three") x 123)
  (get-multi-hash '("one" "two" "three") x)

  (setf (get-multi-hash '("one" "two" "three") x) 321)
  ;; Aliases work w/o explicit defsetf
  (setf (gmh '("one" "two" "three") x) 111)
  (gmh '("one" "two" "three") x)

  ;; Not a hashtable!
  (setf (get-multi-hash '("one" "two" "three" "four") x) 1234)
  (setf (get-multi-hash '("one" "two" "three") x) (make-hash-table :test #'equal))
  (setf (get-multi-hash '("one" "two" "three" "four") x) 1234))
