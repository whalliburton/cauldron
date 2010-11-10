;; testing.lisp

(in-package :utilities)

(defmacro prologue (name 
                    &optional (optimize '((space 0) (speed 0) (safety 3) (debug 3))))
  (let ((sym (symb (if (symbol-starts-with name 'test-)
                     (subseq (symbol-name name) 5) 
                     name) 
                   (unless (symbol-ends-with name '-tests) '-tests))))
    `(progn
       (declaim (optimize ,@optimize))
       (lift:deftestsuite ,sym () ()))))

(defun list-testsuites ()
  (iter (for suite in (lift:testsuites))
        (for name = (symbol-name suite))
        (when (string-ends-with name "-TESTS")
          (collect (symb (subseq name 0 (- (length name) 6)))))))

(defun list-tests (suite)
  (lift:testsuite-tests (symb (concatenate 'string (symbol-name suite) "-TESTS"))))

(defun list-all-tests ()
  (iter (for suite in (list-testsuites))
        (collect (append (list suite) (list-tests suite)))))

(defun describe-test-result (result)
  (lift:describe-test-result result t))

(defun %run-testsuites (&optional suites)
  (if (and (symbolp suites) (not (null suites)))
    (let ((result (lift:run-tests 
                   :suite 
                   (if (symbol-ends-with suites '-tests)
                     suites
                     (symb suites '-tests)))))
      (lift:describe-test-result result t)
      (newline 2))
    (mapc '%run-testsuites (or suites (list-testsuites))))
  (values))

(defmacro run-testsuites (&rest suites)
  (if suites
    `(%run-testsuites ',suites)
    `(%run-testsuites)))

;; (defun find-test-suite (name)
;;   (iter (for (suite . tests) in (list-all-tests))
;;         (when (member name tests) (return (symb suite '-tests)))))

(defun %run-test (&rest tests)
  (let ((result (lift:make-test-result :multiple :multiple)))
    (iter (for test in tests)
          (lift:run-test :name test 
                         :suite (or (find-test-suite test)
                                    (error "Test ~A not found." test))
                         :result result))
    (lift:describe-test-result result t)
    (newline)
    result))

(defmacro run-test (&rest tests)
  `(%run-test ,@(mapcar (lambda (el) (list 'quote el)) tests)))

(defun remove-all-tests ()
  (mapc (lambda (el) (setf (lift:testsuite-tests el) nil)) (lift:testsuites))
  (setf lift::*test-is-being-compiled?* nil
        lift::*test-is-being-loaded?* nil
        lift::*test-is-being-defined?* nil))

(defmacro test (name form &optional (result t))
  `(lift:addtest ,name (lift:ensure-same ,form ,result)))

(defmacro tests (name &rest tests)
  `(list
    ,@(iter (for (form vals) on tests by #'cddr)
            (for x upfrom 1)
            (collect `(test ,(symb name "-" x) ,form ,vals)))))

(defmacro ensure-null (form) 
  `(lift:ensure-same ,form nil))

