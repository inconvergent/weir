(in-package #:weir-tests)

(defun %test-pigment ()

  (do-test (pigment:rgb 0.1d0 1d0 0.5d0)
           #s(pigment:rgba :r 0.1d0 :g 1.0d0 :b 0.5d0 :a 1.0d0))

  (do-test (pigment:to-list (pigment:rgb 0.1d0 1d0 0.5d0 0.2d0))
           (list 0.10000000000000002d0 1.0d0 0.5d0 0.2d0))

  (do-test (pigment:hsv 0.5d0 1d0 1d0) (pigment:rgb 0d0 1d0 1d0))

  (do-test (pigment:to-list (pigment:rgb 0d0 1d0 1d0 0.5d0))
           (list 0.0d0 1.0d0 1.0d0 0.5d0))

  (do-test (pigment:to-list* (pigment:rgb 0d0 1d0 1d0 0.5d0))
           (list 0.0d0 0.5d0 0.5d0 0.5d0))

  (do-test (pigment:cmyk 1d0 0d0 0d0 0d0)
           #s(pigment:rgba :r 0.0d0 :g 1.0d0 :b 1.0d0 :a 1.0d0))

  (do-test (pigment:cmyk 0.5d0 0d0 0d0 0.5d0)
           #s(pigment:rgba :r 0.25d0 :g 0.5d0 :b 0.5d0 :a 1.0d0))

  (do-test (pigment:to-hex (pigment:rgb 1d0 0d0 1d0)) "#FF00FF")

  (do-test (pigment:to-hex (pigment:rgb 1d0 0.5d0 1d0)) "#FF80FF")

  (do-test (pigment:to-hex (pigment:rgb 0d0 0d0 0d0)) "#000000")

  (do-test (pigment:to-hex (pigment:rgb 0d0 0.03d0 0.01d0)) "#000702")

  (do-test (pigment:scale (pigment:rgb 0.8d0 0.8d0 0.8d0 0.4d0) 0.1d0)
           #s(pigment:rgba :r 0.03200000000000001d0
                           :g 0.03200000000000001d0
                           :b 0.03200000000000001d0
                           :a 0.04000000000000001d0))

  (do-test (pigment:scale! (pigment:rgb 0.8d0 0.8d0 0.8d0 0.4d0) 0.1d0)
           #s(pigment:rgba :r 0.03200000000000001d0
                           :g 0.03200000000000001d0
                           :b 0.03200000000000001d0
                           :a 0.04000000000000001d0))

  ;TODO: deal with alpha in add
  (do-test (pigment:safe-clamp!
             (pigment:non-a-add (pigment:rgb 0.8d0 0.8d0 0.8d0 0.4d0)
                          (pigment:red)))
           #s(pigment:rgba :r 1.0d0
                           :g 0.32000000000000006d0
                           :b 0.32000000000000006d0
                           :a 1.0d0))
  ;rgb  51, 102, 178,
  ;hsv 0.597, 0.71, 0.698
  (do-test (pigment:as-hsv (pigment:rgb 0.2d0 0.4d0 0.7d0))
           '(0.6d0 0.7142857142857143d0 0.7d0 1.0d0))

  (do-test (pigment:as-hsv (pigment:rgb 1d0 0.0d0 0.01d0))
           '(0.9983333333333333d0 1.0d0 1.0d0 1.0d0))

  (do-test (pigment:as-hsv (pigment:rgb 0d0 1d0 1d0))
           '(0.5d0 1.0d0 1.0d0 1.0d0)))


(define-file-tests test-pigment ()
  (test-title (%test-pigment)))
