#!/usr/local/bin/sbcl --script

; set your path to sbcl above. i would use env, but it does not appear to work
; with the --script argument. alternately, delete the shebang and the load
; below.  and run from repl. let me know if you have a better suggestion for
; making this easily runnable from terminal

(load "load")


(defun main (size fn)
  (let ((mid (* 0.5d0 size))
        (repeat 15)
        (grains 3)
        (itt 1000)
        (sand (sandpaint:make :size size
                              :fg (pigment:black 0.01d0)
                              :bg (pigment:white))))

    (loop for i in (math:linspace repeat 100d0 900d0)
          for j from 0 to repeat do
      (weir-utils:print-every j 4)
      (let ((wer (weir:make))
            (va (vec:vec 0d0 0d0))
            (vb (vec:vec 0d0 0d0))
            (p1 (vec:vec 100d0 i))
            (p2 (vec:vec 900d0 i)))

        (loop for s in (math:linspace itt 0d0 1d0) do
          (let ((v1 (weir:add-vert! wer (vec:on-line s p1 p2)))
                (v2 (weir:add-vert! wer (vec:add va (vec:on-line s p1 p2)))))

            (setf va (vec:add va (rnd:in-circ (* 0.7d0 j))))
            (setf vb (vec:add vb (rnd:in-circ (* 0.001d0 j))))

            (weir:with (wer %)
              (weir:itr-grp-verts (wer v :collect nil)
                (% (weir:move-vert? v (vec:add (rnd:in-circ 0.1d0) vb))))
              (% (weir:add-edge? v1 v2)))

            (weir:itr-edges (wer e)
              (sandpaint:stroke sand (weir:get-verts wer e) grains))
            (sandpaint:pix sand (weir:get-all-verts wer))))))

    (sandpaint:save sand fn)))

(time (main 1000 (second (weir-utils:cmd-args))))

