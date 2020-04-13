(defpackage #:weir-tests
  (:use #:common-lisp #:weir-utils)
  (:export #:run-tests))

(in-package #:weir-tests)

(defvar *tests*)
(defvar *fails*)
(defvar *passes*)

;TODO: there are still a lot of qirks here. the do test macro and global test
;variables (above) are far from optmal. there is probably a better way to do
;this via ASDF

;TODO the tests that write an output file currently fail when running locally
;(because the path is wrong) they work in the docker image, but the output file
;is never checked (against the files in data)


; TODO: approximately similar to

;https://www.rosettacode.org/wiki/Program_termination#Common_Lisp
(defun terminate (status)
  #+sbcl (sb-ext:quit :unix-status status)
  #+ccl (ccl:quitstatus)
  #+clisp (ext:quitstatus)
  #+cmu (unix:unix-exit status)
  #+ecl (ext:quitstatus)
  #+abcl (ext:quit:status status)
  #+allegro (excl:exitstatus :quiet t)
  #+gcl (common-lisp-user::bye status)
  #+ecl (ext:quitstatus))


(defmacro test-title (&body body)
  `(progn
     (format t "~%~%~%@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@> ~a~%" ',@body)
     ,@body))


(defmacro do-test (a b)
  (alexandria:with-gensyms (aname bname)
    `(let ((,aname ,a)
           (,bname ,b))
      (incf *tests*)
      (if (equalp ,aname ,bname)
        (progn
          (incf *passes*)
          (format t "~%~%~%~a ~%-----------------------------------------> pass" ',a))
        (progn
          (incf *fails*)
          (format t "~%~%~%~a ~%#########################################> fail ~%--  wanted: ~% ~a ~%--  got: ~% ~a~%-----------------------------------------~%"
            ',a ,bname ,aname))))))


(defun %tests-summary ()
  (format t "~% tests:  ~a~% fails:  ~a~% passes: ~a~%"
    *tests* *fails* *passes*)
  (when (> *fails* 0) (print "--- at least one test failed! ---")
                      (terminate 1)))


;;; test running for whole project

(defvar *test-functions* nil)

(defmacro define-file-tests (name () &body body)
  `(progn
     (defun ,name ()
       (handler-case (progn ,@body)
         (error (c)
           (declare (ignore c))
           (warn "! ! ! Errored when running file tests ~A.~%" ',name))))
     (pushnew ',name *test-functions*)))

(defun run-tests ()
  (setf *tests* 0)
  (setf *passes* 0)
  (setf *fails* 0)
  (dolist (test *test-functions*)
    (fresh-line)
    (format t "=============================================~%")
    (format t "============== Running file tests ~A~%" test)
    (format t "=============================================~2%")
    (rnd:set-rnd-state 1)               ; Re-seed state before each
                                        ; test.
    (let ((*print-pretty* t))
      (funcall test)))

    (%tests-summary))

