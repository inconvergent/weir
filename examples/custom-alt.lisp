#!/usr/local/bin/sbcl --script

; set your path to sbcl above. i would use env, but it does not appear to work
; with the --script argument. alternately, delete the shebang and the load
; below.  and run from repl. let me know if you have a better suggestion for
; making this easily runnable from terminal

(load "load")

(defun vert-if-no-collision? (c rad xy)
  (lambda (wer)
    (when (<= (length (weir:verts-in-rad wer xy rad)) 1)
          (weir::-valid-vert ((weir::weir-num-verts wer) c :err nil)
            (weir:add-vert! wer xy)))))

(defun main (fn)
  (let ((rad 7d0)
        (wer (weir:make))
        (psvg (draw-svg:make)))

    (loop repeat 50
          do (loop with curr = (weir:add-vert! wer (rnd:in-circ 200d0
                                                     :xy (vec:rep 500d0)))
                   repeat 100
                   do (weir:build-zonemap wer rad)
                      (weir:with (wer %)
                        (% (vert-if-no-collision? curr rad
                             (rnd:in-circ rad :xy (weir:get-vert wer curr))) :v)
                        (% (weir:add-edge? :v curr) (:v) :e))

                      (let ((res (weir:get-alteration-result-map wer)))
                        (weir-utils:aif (gethash :v res)
                          (progn (setf curr weir-utils::it)))
                        (weir-utils:aif (gethash :e res)
                          (draw-svg:path psvg
                            (weir:get-verts wer weir-utils::it))))))
        (draw-svg:save psvg fn)))


(time (main (second (weir-utils:cmd-args))))

