#!/usr/local/bin/sbcl --script

; set your path to sbcl above. i would use env, but it does not appear to work
; with the --script argument. alternately, delete the shebang and the load
; below.  and run from repl. let me know if you have a better suggestion for
; making this easily runnable from terminal

(load "load")

(defun -dot-split (wer e xp plane-point plane-vec g)
  (weir:ldel-edge! wer e :g g)
  (destructuring-bind (a b) e
    (if (> (vec:3dot plane-vec (vec:3sub (weir:3get-vert wer a) plane-point)) 0d0)
      (progn (weir:add-edge! wer (weir:3add-vert! wer xp) b :g g)
             (weir:add-edge! wer (weir:3add-vert! wer xp) a :g g))
      (progn (weir:add-edge! wer (weir:3add-vert! wer xp) a :g g)
             (weir:add-edge! wer (weir:3add-vert! wer xp) b :g g)))))


(defun plane-intersect (wer &key plane-point plane-vec (a 0d0) (dst 0d0) dot-split g)
  (let* ((rv (hset:make))
         (do-split (if dot-split
                       (lambda (e xp)
                         (-dot-split wer e xp plane-point plane-vec g))
                       (lambda (e xp)
                         (list (weir:3lsplit-edge! wer e :xy xp))))))
    (weir:itr-grp-verts (wer v :g g)
      (when (> (vec:3dot plane-vec (vec:3sub (weir:3get-vert wer v)
                                             plane-point))
               0d0)
            (hset:add rv v)))

    (weir:itr-edges (wer e :g g)
      (multiple-value-bind (x d xp) (vec:3planex plane-vec plane-point
                                                 (weir:3get-verts wer e))
        (when (and x (< 0d0 d 1d0))
          (hset:add* rv  (funcall do-split e xp)))))

    (weir:3transform! wer (hset:to-list rv)
      (lambda (vv) (vec:3ladd* (vec:3lrot* vv plane-vec a :xy plane-point)
                               (vec:3smult plane-vec dst))))))



(defun get-width (d)
  (* 4d0 (expt (- 1d0 (max 0d0 (min (/ d 1500d0) 1d0))) 2d0)))


(defun dst-draw (wer proj psvg)
  (loop with lr = (line-remove:make :cnt 8 :rs 2d0 :is 1.5d0)
        for e in (weir:get-edges wer)
        do (let* ((point-dst (ortho:project* proj (weir:3get-verts wer e)))
                  (dsts (math:lpos point-dst :fx #'second)))
             (loop for path across
                     (line-remove:path-split lr
                       (list (cpath:cpath (weir-utils:to-vector (math:lpos point-dst))
                                          (loop for d in dsts collect (get-width d))
                                          (ceiling (* 2.2d0 (get-width (apply #'max dsts)))))))
                   do (draw-svg:path psvg path :sw 0.8d0 :so 0.9d0 :stroke "black")))
        finally (print (line-remove:stats lr))))


(defun init-cube (n m s)
  (let* ((dots (weir-utils:to-vector (rnd:3nin-cube m s)))
         (dt (kdtree:make (weir-utils:to-list dots)))
         (res (weir-utils:make-adjustable-vector)))

    (loop for rad in (rnd:nrnd 3 500d0)
          for mid in (math:nrep 3 (rnd:3on-sphere :rad 300d0))
          do (loop for p in (math:nrep n (rnd:3on-sphere :rad rad :xy mid))
                   if (> (vec:3dst p (aref dots (kdtree:nn dt p )))
                         90d0)
        do (weir-utils:vextend p res)))
    (weir-utils:to-list res)))


(defun main (size fn)
  (let* ((psvg (draw-svg:make*))
         (wer (weir:make :dim 3 :max-verts 200000))
         (mid (vec:rep 500d0))
         (st (state:make))
         (proj (ortho:make :s 0.5d0
                           :xy mid
                           :cam (rnd:3on-sphere :rad 1000d0)
                           :look vec:*3zero*)))

     (loop repeat 2 do (plane-intersect wer
                         :plane-point (rnd:3in-cube 500d0)
                         :plane-vec (rnd:3on-sphere)
                         :a (rnd:rnd 0.5d0) :dst 100d0
                         :dot-split t))

    (weir:3add-verts! wer (init-cube 7000 800 500d0))

    (weir:3relative-neighborhood! wer 500d0)

    (loop repeat 2 do (plane-intersect wer :plane-point (rnd:3in-cube 300d0)
                                           :plane-vec (rnd:3on-sphere)
                                           :a (rnd:rnd 0.3d0)
                                           :dst 250d0
                                           :dot-split t))

    (dst-draw wer proj psvg)
    (draw-svg:save psvg fn)))

(time (main 1000 (second (weir-utils:cmd-args))))

