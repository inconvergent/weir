#!/usr/local/bin/sbcl --script

; set your path to sbcl above. i would use env, but it does not appear to work
; with the --script argument. alternately, delete the shebang and the load
; below.  and run from repl. let me know if you have a better suggestion for
; making this easily runnable from terminal

(load "load")

; custom alteration
; add new vert inside rad of c if there is no collision
(defun vert-if-no-collision? (c rad xy)
  (lambda (wer)
    (when (<= (length (weir:verts-in-rad wer xy rad)) 1)
          (weir::-valid-vert ((weir::weir-num-verts wer) c :err nil)
            (weir:add-vert! wer xy)))))


; some kind of non-colliding random walk
(defun random-walk (wer psvg rad)
  (loop with curr = (weir:add-vert! wer
                      (rnd:in-circ 200d0 :xy (vec:rep 500d0)))
        repeat 100
        do (weir:build-zonemap wer rad)
           (weir:with (wer %)
             (% (lambda (_) (print :e)) (:e))
             ; the result of this alteration will be available
             ; inside this context as :v
             (% (vert-if-no-collision? curr rad
                  (rnd:in-circ rad
                    :xy (weir:get-vert wer curr))) :v)
             ; this alteration references (:v), and the result will
             ; be avilable as :e (it is used below)
             (% (weir:add-edge? :v curr) (:v) :e)

             ; lambdas are allowed as well.
             ; draw new edge, and update curr.
             ; if you provide an argument to lambda it will be wer when it is
             ; executed
             (% (lambda () (progn (setf curr :v)
                                  (draw-svg:path psvg
                                    (weir:get-verts wer :e))))
                (:v :e)))))


(defun main (fn)
  (let ((rad 7d0)
        (wer (weir:make))
        (psvg (draw-svg:make)))
    (loop repeat 50 do (random-walk wer psvg rad))
    (draw-svg:save psvg fn)))


(time (main (second (weir-utils:cmd-args))))

