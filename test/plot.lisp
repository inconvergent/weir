#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(setf *print-pretty* t)


(defun main (size fn)
  (let ((p (list (vec:vec 100d0 100d0) (vec:vec 200d0 100d0)
                 (vec:vec 200d0 200d0)))
        (psvg (draw-svg:make*)))

    (draw-svg:show-boundary psvg)
    (draw-svg:show-crop psvg :len 20d0)

    (draw-svg:path psvg p)
    (draw-svg:path psvg (vec:ladd* p (vec:vec 200d0 0d0)) :closed t)
    (draw-svg:path psvg (vec:ladd* p (vec:vec 400d0 0d0)) :closed t :sw 10d0)
    (draw-svg:path psvg (vec:ladd* p (vec:vec 600d0 0d0))
                   :closed t :fill "black")

    (draw-svg:bzspl psvg (vec:ladd* p (vec:vec 0d0 200d0)))
    (draw-svg:bzspl psvg (vec:ladd* p (vec:vec 200d0 200d0)) :closed t)
    (draw-svg:bzspl psvg (vec:ladd* p (vec:vec 400d0 200d0)) :closed t :sw 10d0)
    (draw-svg:bzspl psvg (vec:ladd* p (vec:vec 600d0 200d0)) :closed t
                    :fill "black")

    (draw-svg:circ psvg (vec:vec 100d0 500d0) 20d0)
    (draw-svg:circ psvg (vec:vec 200d0 500d0) 20d0 :sw 10d0)
    (draw-svg:circ psvg (vec:vec 300d0 500d0) 20d0 :sw 10d0 :fill "black")
    (draw-svg:circ psvg (vec:vec 400d0 500d0) 20d0 :aspath t)

    (draw-svg:carc psvg (vec:vec 500d0 500d0) 20d0 0d0 PI5 :sw 2d0)
    (draw-svg:circ psvg (vec:vec 500d0 500d0) 15d0)

    (draw-svg:carc psvg (vec:vec 560d0 500d0) 20d0 0d0 4d0 :sw 3d0)
    (draw-svg:circ psvg (vec:vec 560d0 500d0) 15d0)

    (draw-svg:carc psvg (vec:vec 620d0 500d0) 20d0 0.5d0 1d0 :sw 4d0)
    (draw-svg:circ psvg (vec:vec 620d0 500d0) 15d0)

    (draw-svg:carc psvg (vec:vec 700d0 500d0) 20d0 3.4d0 5d0 :sw 5d0)
    (draw-svg:circ psvg (vec:vec 700d0 500d0) 15d0)

    (draw-svg:wcirc psvg (vec:vec 800d0 500d0) 15d0 :rs 0.7d0)
    (draw-svg:wcirc psvg (vec:vec 850d0 500d0) 15d0 :outer-rad 20d0 :rs 0.7d0)

    (draw-svg:wpath psvg (vec:ladd* p (vec:vec 0d0 600d0)) :width 10d0 :rs 0.5d0)
    (draw-svg:wpath psvg (vec:ladd* p (vec:vec 200d0 600d0)) :width 10d0 :rs 0.5d0
                    :cap nil)
    (draw-svg:wpath psvg (vec:ladd* p (vec:vec 400d0 600d0)) :width 10d0 :rs 0.5d0
                    :cap nil
                    :opposite nil)

    ;(draw-svg:compound psvg (list (list :path (list (vec:vec 700d0 700d0)
    ;                                                (vec:vec 720d0 730d0)
    ;                                                (vec:vec 730d0 710d0)))
    ;                              (list :bzspl (list (vec:vec 700d0 700d0)
    ;                                                 (vec:vec 720d0 730d0)
    ;                                                 (vec:vec 730d0 710d0)))))

    (draw-svg:save psvg "data/plot")))

(time (main 1000 (second (cmd-args))))

