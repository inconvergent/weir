#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(load "../utils/test")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun -dst (verts cand)
  (loop with hit = 0
        with dst = (vec:3dst2 (first verts) cand)
        for v in (cdr verts)
        for i from 1
        do (let ((d (vec:3dst2 v cand)))
             (when (< d dst) (setf dst d hit i)))
        finally (return hit)))

(defun -rad (verts cand rad)
  (loop with rad2 = (expt rad 2d0)
        with res = (make-adjustable-vector :type 'fixnum)
        for v in verts
        for i from 0
        do (let ((d (vec:3dst2 v cand)))
             (when (< d rad2) (vextend i res)))
        finally (return res)))


(defun test-kdtree ()
  (let* ((verts (rnd:3nin-cube 100000 500d0))
         (kd (time (kdtree:make verts)))
         (cands (rnd:3nin-cube 10000 500d0)))

    ;(print kd)

    (do-test (time (loop for cand in cands collect (kdtree:nn kd cand)))
             (time (loop for cand in cands collect (-dst verts cand))))

    (do-test (time (sort (kdtree:rad kd (vec:3vec 100d0 200d0 41d0) 50d0) #'<))
             (time (sort (-rad verts (vec:3vec 100d0 200d0 41d0) 50d0) #'<)))))



(defun main ()
  (test-title (test-kdtree))
  (test-title (test-summary)))


;(require :sb-sprof)
;(sb-sprof:with-profiling (:max-samples 200000
;                         ;:mode :cpu
;                         ;:mode :alloc
;                         :mode :time
;                         :report :graph)
; (main))

(main)
