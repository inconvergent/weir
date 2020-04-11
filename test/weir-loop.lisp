(in-package #:weir-tests)

;(defvar *r* (rnd:rndi 10000))

;(print *r*)

(defun %test-weir-loop (size)
  (let* ((mid (vec:rep (* 0.5d0 size)))
         (psvg (draw-svg:make* :height size :width size))
         (wer (weir:make)))

    (weir:add-verts! wer (bzspl:adaptive-pos
                           ; use n = 20 (*r*=76) to get some overlaps
                           ; TODO: fix this case
                           (bzspl:make (rnd:nin-circ 10 400d0 :xy mid))
                           :lim 2d0))

    (weir:relative-neighborhood! wer 500d0)
    (weir:center! wer :xy mid)

    (weir:with (wer %)
      (weir:itr-edges (wer e)
        (draw-svg:path psvg (weir:get-verts wer e)
                             :sw 2d0
                             :stroke "black"
                             :so 0.3d0)))

    (loop for edge in (weir:get-min-spanning-tree wer :start 0 :edges t)
          do
             (draw-svg:path psvg (weir:get-verts wer edge)
                            :stroke "red"
                            :sw 3d0
                            :so 1d0))

    (loop for lp in (weir:get-cycle-basis wer)
          do (print lp)
          (draw-svg:path psvg (weir:get-verts wer (progn lp))
                         :sw 0.5d0)
          (draw-svg:hatch psvg (weir:get-verts wer (progn lp))
                          :angles (list (rnd:rnd* PI))
                          :closed t
                          :sw 0.3d0
                          :rs 1d0))

    (draw-svg:save psvg "data/weir-loops")))

(define-file-tests test-weir-loop ()
  ;; TODO: other cases: 8407 4445
  (rnd:set-rnd-state 76)
  (time (%test-weir-loop 600d0)))
