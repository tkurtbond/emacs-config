;;; srec-test.el --

;; emacs -batch -l ert -l srec-test.el -f ert-run-tests-batch-and-exit

;;; Commentary:

;;; Code:

(load-file "srec.el")

(srec-define person
             first
             last
             age
             sex)

(let ((a (make-person)))
  (person-last! a "bond")
  (pp a)
)


(ert-deftest make-person ()
  (let ((a (make-person)))
    (should (person-p a))))


(ert-deftest internal-fields ()
  (should (equal '(:first :last :age :sex) srec--person-fields)))


(ert-deftest access-person ()
  (let ((a (make-person)))
    (should (null (person-last a)))
    (person-last! a "bond")
    (should (equal (person-last a) "bond"))
    (person-last! a "smith")
    (should (equal (person-last a) "smith"))))

