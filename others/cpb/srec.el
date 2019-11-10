(require 'cl)


(defmacro srec-define (name &rest fields)
  (let ((type-marker (intern (format ":%s" name)))
        (predicate (format "%s-p" name))
        (fields-symbol (intern (format "srec--%s-fields" name))))

    `(progn
       (defvar ,fields-symbol (list ,@(mapcar (lambda (x) (intern (format ":%s" x))) fields)))
       (defun ,(intern predicate) (rec) (eq (car rec) ,type-marker))
       (defun ,(intern (format "%s-copy" name)) (rec) (cl-assert (eq (car rec) ,type-marker)) (copy-sequence rec)) 
       ;; TODO Support field names at make time.
       (defun ,(intern (format "make-%s" name)) () (list ,type-marker))
       ,@(srec-make-accessors name fields type-marker)
       ,@(srec-make-setters name fields type-marker))))


(defun srec-make-accessors (name fields type-marker)
  (if (null fields)
      nil
    (let ((accessor (intern (format "%s-%s" name (car fields))))
          (field (intern (format ":%s" (car fields)))))

      (cons `(defun ,accessor (rec)
               (cl-assert (eq (car rec) ,type-marker))
               (cdr (assoc ,field (cdr rec))))

            (srec-make-accessors name (cdr fields) type-marker)))))


(defun srec-make-setters (name fields type-marker)
  (if (null fields)
      nil
    (let ((setter (intern (format "%s-%s!" name (car fields))))
          (field (intern (format ":%s" (car fields)))))

      (cons `(defun ,setter (rec value)
               (cl-assert (eq (car rec) ,type-marker))
               (let ((v (assoc ,field (cdr rec))))
                 (if v
                     (setf (cdr v) value)
                   (setf (cdr rec) (cons (cons ,field value) (cdr rec))))))

            (srec-make-setters name (cdr fields) type-marker)))))


;; (cl-loop for (key value) on '(:prop1 a :prop2 b :prop3 c) by 'cddr
;;         collect value)
;;; (a b c)


;; (defun prop-values (plist)
;;   "..."
;;   (let ((pl    (cdr plist))
;;         (vals  ()))
;;     (while pl
;;       (push (car pl) vals)
;;       (setq pl  (cddr pl)))
;;     (nreverse vals)))


;; (srec-define person (first last age))

(provide 'srec)
