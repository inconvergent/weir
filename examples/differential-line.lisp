#!/usr/local/bin/sbcl --script

; set your path to sbcl above. i would use env, but it does not appear to work
; with the --script argument. alternately, delete the shebang and the load
; below.  and run from repl. let me know if you have a better suggestion for
; making this easily runnable from terminal

(load "load")

(defvar signs (list 1d0 -1d0))


(defun init (wer xy)
  (weir:add-path! wer (vec:polygon 10 20d0 :xy xy) :closed t))

(defun attract (f s attract)
  (vec:smult (vec:norm f) (* s attract)))

(defun reject (rad reject d &aux (l (vec:len d)))
  (vec:smult d (* reject (- (/ rad l) 1d0))))

(defun jitter (wer r)
  (weir:itr-verts (wer i)
    (weir:move-vert! wer i (rnd:in-circ r))))


(defun do-step (wer &key attract reject near-limit split-limit rad)
  (progn (weir:with (wer % :db nil)
    ; attract
    (weir:itr-edge-verts* (wer e v)
      (let ((f (apply #'vec:isub v)))
        (loop for i in e and s in signs
              if (> (vec:len f) near-limit)
              do (% (weir:move-vert? i (attract f s attract))))))
    ; reject
    (weir:itr-verts (wer v)
      (loop with near = (weir:verts-in-rad wer (weir:get-vert wer v) rad)
            for w across near
            if (not (= w v))
            do (% (weir:move-vert? v
                    (reject rad reject
                      (apply #'vec:sub (weir:get-verts wer (list v w))))))))
    ; split
    (weir:itr-edge-verts* (wer e v)
      (when (> (apply #'vec:dst v) split-limit)
            (% (weir:lsplit-edge? e :xy (vec:lmid v))))))))


(defun draw (wer fn &optional i)
  (let ((psvg (draw-svg:make*))
        (sw 1d0)
        (fill (pigment:to-hex (pigment:gray 0.13d0))))
    (weir:itr-edges (wer e)
      (draw-svg:wpath psvg (weir:get-verts wer e) :stroke "black"
                     :so 0.95d0 :width 2 :rs 0.8 :sw sw))
    (draw-svg:save psvg (if i (weir-utils:append-number fn i) fn))))


(defun main (size fn)
  (let* ((xy (vec:rep 500d0))
         (wer (weir:make :max-verts 100000 :adj-size 100000 :set-size 2))
         (attract 0.0002d0)
         (reject 0.007d0)
         (near-limit 3d0)
         (split-limit 8d0)
         (rad 20d0))

    (init wer xy)
    (jitter wer 3d0)

    (loop repeat 1000
          for i from 0
          do (weir-utils:print-every i 100)
             (weir:build-zonemap wer rad)
             (do-step wer :attract attract :near-limit near-limit
                          :reject reject :split-limit split-limit :rad rad)
             (jitter wer 0.01d0))

    (draw wer fn)))

(time (main 1000 (second (weir-utils:cmd-args))))

