#!/usr/bin/sbcl --script

(load "../examples/load")

;TODO: integrate this into the test suite

(rnd:set-rnd-state 1)


(defun main (size fn)
  (let ((sand (sandpaint:make :size size
                :fg (pigment:white)
                :bg (pigment:white))))

    (sandpaint:clear-fx sand :fx (lambda (xy)
                               (pigment:hsv 0d0 0d0 (+ 0.9d0 (rnd:rnd 0.1d0)))))

    (loop with ls = (math:linspace 9 200d0 1800d0)
          for x in ls
          for i from 0
          do (loop for y in ls
                   for j from 0
                   if (>= i j)
                   do

        (sandpaint:set-fg sand (pigment:black 0.3d0))
        (sandpaint:circ sand (list (vec:vec x y)) 120d0 300000)

        (sandpaint:set-fg sand (pigment:white 0.1d0))
        (sandpaint:circ sand (list (vec:vec x y)) 100d0 300000)

        (sandpaint:set-fg sand (pigment:white))
        (sandpaint:circ sand (list (vec:vec x y)) 80d0 300000)
        ))

    (sandpaint:set-fg sand (pigment:black 0.3d0))
    (sandpaint:circ sand (list (vec:rep 1000d0)) 5d0 30000)


    (loop for y in (math:linspace 4 100d0 1000d0)
          do (loop for x in (math:linspace 200 (+ y 100d0) (+ y 300d0))
          do (sandpaint:stroke sand (list (vec:vec x (- 1700d0 x))
                                          (vec:vec x (+ x 1800d0)))
                               1000)))

    (sandpaint:chromatic-aberration sand
      :cafx (sandpaint:cafx-expt (vec:rep 1000d0) 1000d0 5d0))

    (sandpaint::check-integrity sand)

    (sandpaint:save sand "chromatic")))


(time (main 2000 (second (weir-utils:cmd-args))))

