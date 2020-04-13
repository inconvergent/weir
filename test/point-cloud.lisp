(in-package #:weir-tests)

(defun make-data (n s)
  (loop with ls = (print (math:linspace n (- s) s))
        with res = (list)
        with c = (vec:3vec 0.1d0 0.4d0 0.9d0)
        for x in ls
        do (loop for y in ls
                 do (loop for z in ls
                          do (push (list (vec:3vec x y z) c) res)))
        finally (return res)))


(defun %test-point-cloud ()

  (let* ((data (make-data 10 20d0))
        (ptc (point-cloud:make :data data))
        (bvh (point-cloud:make-bvh ptc :num 10 :rad 2d0))
        (raycastfx (point-cloud:make-raycaster bvh)))

    (do-test (loop for x in (math:linspace 4 -10d0 10d0)
                   collect (funcall raycastfx
                                    (list (vec:3vec x 2d0 99d0)
                                          (vec:3vec (+ x 0.1d0) 1d0 -100d0))))
             '(#s(point-cloud:bvhres :s 77.48330522978203d0 :i 740)
               #s(point-cloud:bvhres :s 77.42723594449721d0 :i 540)
               #s(point-cloud:bvhres :s 77.48330522978203d0 :i 440)
               #s(point-cloud:bvhres :s 77.42723594449721d0 :i 240)))

    (do-test (funcall raycastfx (list (vec:3vec 2.22d0 -99d0 2.22d0 )
                                      (vec:3vec 2.223d0 100d0 2.223d0)))
             '#s(point-cloud:bvhres :s 77.00000058080441d0 :i 494))))


(define-file-tests test-point-cloud ()
  (test-title (%test-point-cloud)))
