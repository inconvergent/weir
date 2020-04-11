#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(load "../utils/test")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)

(defun make-data (n s)
  (loop with ls = (print (math:linspace n (- s) s))
        with res = (list)
        with c = (vec:3vec 0.1d0 0.4d0 0.9d0)
        for x in ls
        do (loop for y in ls
                 do (loop for z in ls
                          do (push (list (vec:3vec x y z) c) res)))
        finally (return res)))


(defun test-point-cloud ()

  (let* ((data (make-data 10 20d0))
        (ptc (point-cloud:make :data data))
        (bvh (point-cloud:make-bvh ptc :num 10 :rad 2d0))
        (raycastfx (point-cloud:make-bvh-raycaster bvh)))

    (do-test (loop for x in (math:linspace 4 -10d0 10d0)
                   collect (funcall raycastfx
                                    (list (vec:3vec x 2d0 99d0)
                                          (vec:3vec (+ x 0.1d0) 1d0 -100d0))))
             '(#s(bvh-utils::bvh-result :s 77.48330522978203d0 :hit 740
                   :pt #s(vec:3vec :x -11.11111111111111d0 :y 2.2222222222222214d0 :z 20.0d0))
                #s(bvh-utils::bvh-result :s 77.42723594449721d0 :hit 540
                   :pt #s(vec:3vec :x -2.2222222222222214d0 :y 2.2222222222222214d0 :z 20.0d0))
                #s(bvh-utils::bvh-result :s 77.48330522978203d0 :hit 440
                   :pt #s(vec:3vec :x 2.2222222222222214d0 :y 2.2222222222222214d0 :z 20.0d0))
                #s(bvh-utils::bvh-result :s 77.42723594449721d0 :hit 240
                   :pt #s(vec:3vec :x 11.111111111111114d0 :y 2.2222222222222214d0 :z 20.0d0))))

    (do-test (funcall raycastfx (list (vec:3vec 2.22d0 -99d0 2.22d0 )
                                      (vec:3vec 2.223d0 100d0 2.223d0)))
             '#s(bvh-utils::bvh-result :s 77.00000058080441d0 :hit 494
                  :pt #s(vec:3vec :x 2.2222222222222214d0 :y -20.0d0 :z 2.2222222222222214d0)))))


(defun main ()
  (test-title (test-point-cloud))
  (test-title (test-summary)))

(main)

