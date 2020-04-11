(in-package #:weir-tests)

(defun %test-vec ()

  (do-test (vec:norm (vec:vec 3.0d0 0.0d0))
           (vec:vec 1.0d0 0.0d0))

  (do-test (vec:sub (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 3.0d0))
           (vec:vec -1.d0 -1.d0))

  (do-test (vec:add (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 3.0d0))
           (vec:vec 3.0d0 5.0d0))

  (do-test (vec:nsub (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 10.0d0))
           (vec:vec -0.12403473458920847d0 -0.9922778767136677d0))

  (do-test (vec:len2 (vec:vec 1.0d0 2.0d0)) 5)

  (do-test (vec:len (vec:vec 1.0d0 2.0d0)) 2.23606797749979d0)

  (do-test (vec:len (vec:vec 1.0d0 2.0d0)) 2.23606797749979d0)

  (do-test (vec:dst (vec:vec 1.0d0 2.0d0) (vec:vec 1.0d0 3.0d0)) 1.0d0)

  (do-test (vec:mid (vec:vec 1.0d0 2.0d0) (vec:vec 3.0d0 4.0d0))
           (vec:vec 2.0d0 3.0d0))

  (do-test (vec:lsum (list (vec:vec 1.0d0 2.0d0) (vec:vec 0.5d0 4.322d0)))
           (vec:vec 1.5d0 6.322d0))

  (do-test (vec:on-line 0.34d0 (vec:vec 33d0 88d0) (vec:vec 32d0 733d0))
           (vec:vec 32.66d0 307.3d0))

  (do-test (vec:on-circ 0.34d0 388d0 :xy (vec:vec 32d0 733d0))
           (vec:vec -175.9007964518508d0 1060.5992350947818d0))

  (do-test
    (vec:segdst (list (vec:vec 0d0 0d0) (vec:vec 100d0 0d0)) (vec:vec 0d0 200d0))
    200d0)

  (do-test
    (vec:segdst (list (vec:vec 0d0 0d0) (vec:vec 100d0 3d0)) (vec:vec 41d0 202d0))
    200.67971443818558d0)

  (do-test
    (multiple-value-bind (x s)
      (vec:segx (list (vec:rep 1.1d0) (vec:vec 11d0 12.3d0))
                (list (vec:vec 0.1d0 10d0) (vec:vec 8d0 -1.1d0)))
      (list x s))

    (list t 0.2984826334627212d0))

  (do-test
    (vec:segx (list (vec:vec 0d0 0d0) (vec:vec 100d0 0d0))
              (list (vec:vec 0d0 1d0) (vec:vec 100d0 1d0)))
    nil)

  (do-test
    (vec:segx (list (vec:vec 0d0 0d0) (vec:vec 1d0 1d0))
              (list (vec:vec 0d0 1d0) (vec:vec 1d0 0d0)))
    t)

  (do-test (vec:cross (vec:vec 1d0 2d0) (vec:vec 3d0 -7.1d0)) -13.1d0))


(defun %test-3vec ()

  (do-test (vec:3len (vec:3vec 1d0 1d0 0d0)) 1.4142135623730951d0)

  (do-test (vec:3dst (vec:3vec 1d0 1d0 0.5d0) (vec:3vec 0d0 2d0 1.5d0))
           1.7320508075688772d0)

  (do-test (vec:3cross (vec:3vec 0d0 1d0 0.5d0) (vec:3vec 0d0 2d0 1.5d0))
           (vec:3vec 0.5d0 0d0 0d0))

  (do-test (multiple-value-bind (x d p)
             (vec:3planex (vec:3vec 0d0 0d0 1d0) vec:*3zero*
                          (list (vec:3vec 1d0 1d0 -1d0)
                                (vec:3vec 1d0 1d0 1d0)))
             (list x d p))
           (list t 0.5d0 (vec:3vec 1.0d0 1.0d0 0.0d0)))

  (do-test (multiple-value-bind (x hits)
             (vec:3spherex (vec:3zero) 1d0 (list (vec:3rep 2d0) (vec:3rep -2d0)))
             (list x hits))
           (list t (list 0.6443375672974073d0 0.3556624327025929d0))))


(define-file-tests test-vec ()
  (test-title (%test-vec))
  (test-title (%test-3vec))
  (test-title (test-summary)))
