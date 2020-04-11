#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun main (size fn)
  (let ((psvg (draw-svg:make* :width 1000d0
                              :height 1000d0
                              :stroke-width 1d0
                              :rep-scale 0.5d0)))

    (loop for x in (math:linspace 7 80d0 920d0) do
      (loop for y in (math:linspace 7 80d0 920d0) do
        (let ((path (rnd:nin-rect 5 40d0 40d0 :xy (vec:vec x y))))
          (draw-svg:path psvg (vec:ladd* path (vec:vec 20d0 0d0)))
          (draw-svg:cpath psvg (to-list (simplify-path:simplify
                                          (to-vector
                                            (vec:ladd* path (vec:vec -20d0 0d0))
                                            :type 'vec:vec)
                                          :lim 10d0))
                          :width 10d0))))

    (draw-svg:save psvg "data/plot-simplify")))

(time (main 1000 (second (cmd-args))))

