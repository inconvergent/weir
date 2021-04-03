(in-package #:weir-tests)

(defun %main-plot-paths ()

  (rnd:set-rnd-state 76)

  (let* ((size 1000d0)
         (mid (vec:rep (* 0.5d0 size)))
         (psvg (draw-svg:make* :height size :width size))
         (wer (weir:make)))

    (weir:add-path! wer (bzspl:adaptive-pos
                           (bzspl:make (rnd:nin-circ 5 400d0 :xy mid))
                           :lim 2d0)
                    :closed t)

    (weir:add-path! wer (bzspl:adaptive-pos
                           (bzspl:make (rnd:nin-circ 5 400d0 :xy mid))
                           :lim 2d0)
                    :closed t)

    (weir:intersect-all! wer)

    (loop for lp in (weir:get-segments wer)
          do (draw-svg:path psvg (weir:get-verts wer lp)
                            :stroke "red" :sw 5d0))

    (loop for lp in (weir:walk-graph wer)
          do (draw-svg:path psvg (weir:get-verts wer lp)
                            :sw 1d0))

    (draw-svg:save psvg "test/data/plot-paths")))

(define-file-tests test-plot-paths ()
  (time (%main-plot-paths)))

