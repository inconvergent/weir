(in-package #:weir-tests)

(defun %test-math ()
  (do-test (to-list
             (math:path-tangents (list (vec:vec 1.0d0 2.0d0)
                                       (vec:vec 1.0d0 2.0d0)
                                       (vec:vec 0.5d0 4.322d0))))
           (list (vec:rep 0d0)
                 (vec:vec -0.21050655592417808d0 0.977592445711883d0)))

  (do-test (math:imod 20 3 21) 2)

  (do-test (math:dmod 20d0 3d0 21d0) 2d0)

  (do-test (math:linspace 1 0d0 10d0) (list 0.0))

  (do-test (math:linspace 3 0d0 10d0) (list 0.0 5.0 10.0))

  (do-test (math:linspace 2 0d0 10d0 :end nil) (list 0.0 5.0))

  (do-test (math:linspace 2 0d0 10d0 :end t) (list 0.0 10.0))

  (do-test (math:range 2 5) (list 2 3 4))

  (do-test (math:range 5) (list 0 1 2 3 4))

  (do-test (math:argmax '(4 2 3 0 6)) '(4 6))
  (do-test (math:argmax '(4 2 10 0 6)) '(2 10))

  (do-test (math:argmin '(4 2 3 0 6)) '(3 0))
  (do-test (math:argmin '(-1 2 10 4 9 6)) '(0 -1))

  (do-test
    (let ((a (list)))
      (math:with-linspace (10 0d0 7d0 v)
        (setf a (append a (list v))))
      a)
   '(0.0d0 0.7777777777777778d0 1.5555555555555556d0 2.3333333333333335d0
     3.111111111111111d0 3.888888888888889d0 4.666666666666667d0
     5.444444444444445d0 6.222222222222222d0 7.0d0))

  (do-test
    (let ((a (list)))
      (math:with-linspace (10 0d0 7d0 v :end nil)
        (setf a (append a (list v))))
      a)
   '(0.0d0 0.7d0 1.4d0 2.0999999999999996d0 2.8d0 3.5d0 4.199999999999999d0
     4.8999999999999995d0 5.6d0 6.3d0)))



(define-file-tests test-math ()
  (test-title (%test-math)))
