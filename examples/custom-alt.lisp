#!/usr/local/bin/sbcl --script

; set your path to sbcl above. i would use env, but it does not appear to work
; with the --script argument. alternately, delete the shebang and the load
; below.  and run from repl. let me know if you have a better suggestion for
; making this easily runnable from terminal

(load "load")

(defun circ-stroke (sand vv)
  (sandpaint:circ sand
    (lin-path:pos* (lin-path:make vv) (math:linspace 100 0d0 0.99d0))
    1d0 20))


(defun draw-path (sand fn n rad mid)
  (let ((curr nil)
        (cyan (pigment:rgb 0d0 0.7d0 0.7d0 0.01d0))
        (black (pigment:black 0.01d0))
        (i 0))

    (labels
      ((draw (wer xy ee)
        (incf i)
        (setf curr (second ee))
        (sandpaint:set-fg sand cyan)
        (sandpaint:circ sand (list xy) 4d0 3000)
        (sandpaint:set-fg sand black)
        (circ-stroke sand
          (weir:get-verts wer ee))
        (sandpaint:save sand
          (format nil "~a-~3,'0d" fn i))))

      (let ((wer (weir:make :max-verts n)))

        (setf curr (weir:add-vert! wer mid))

        (loop repeat n do
          (weir:with (wer % :zwidth rad)
            (%
              (let* ((c curr)
                     (xy (vec:add (weir:get-vert wer c) (rnd:in-circ rad)))
                     (l (length (weir:verts-in-rad wer xy rad))))
                (lambda (wer)
                  (when (<= l 1)
                        (weir::-valid-vert ((weir::weir-num-verts wer) c :err nil)
                          (let ((w (weir:add-vert! wer xy)))
                            (weir:add-edge! wer c w)
                            (draw wer xy (list c w))
                            w))))))))))))


(defun main (size fn)
  (let ((sand (sandpaint:make :size size
                              :fg (pigment:black 0.01d0)
                              :bg (pigment:white))))

    (draw-path sand fn 5000 10d0 (vec:vec 250d0 250d0))))

(time (main 500 (second (weir-utils:cmd-args))))

