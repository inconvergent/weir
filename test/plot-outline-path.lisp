#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(setf *print-pretty* t)


(defun main (size fn)
  (let ((psvg (draw-svg:make*)))

    (draw-svg:path psvg (cpath:outline (list (vec:vec 100d0 300d0)
                                             (vec:vec 300d0 300d0)
                                             (vec:vec 300d0 100d0)
                                             (vec:vec 100d0 100d0))
                                       (list 20d0 30d0 40d0 20d0)))

    (draw-svg:path psvg (cpath:outline (list (vec:vec 600d0 500d0)
                                             (vec:vec 500d0 500d0)
                                             (vec:vec 500d0 600d0)
                                             (vec:vec 600d0 600d0))
                                       (list 20d0 30d0 70d0 20d0)
                                       :closed t))

    (loop for path in (to-list (hatch:hatch
                                 (to-vector (cpath:outline
                                              (list (vec:vec 800d0 700d0)
                                                    (vec:vec 700d0 700d0)
                                                    (vec:vec 700d0 800d0)
                                                    (vec:vec 800d0 800d0))
                                              (list 10d0 10d0 30d0 10d0)
                                              :closed t))
                                 :angles (list 0d0 PI5)))
          do (draw-svg:path psvg path))


    (draw-svg:save psvg "data/plot-outline-path")))

(time (main 1000 (second (cmd-args))))

