
(in-package :hatch)


(defun stitch (lines)
  "
  randomly mix the hatches in lines according to where the lines intersect.
  this is somewhat inefficient
  "
  (loop with res = (make-adjustable-vector)
        for i from 0 below (length lines)
        do (let ((ss (make-adjustable-vector))
                 (curr (aref lines i)))

             (vextend 0d0 ss)
             (vextend 1d0 ss)

             (loop for j from 0 below (length lines)
                   do (multiple-value-bind (x s)
                        (vec:segx curr (aref lines j))
                        (if x (vextend s ss))))

             (setf ss (sort ss (rnd:either #'< #'>)))

             (loop for k from (rnd:rndi 2) below (1- (length ss)) by 2
                   do (vextend (list (vec:lon-line (aref ss k) curr)
                                     (vec:lon-line (aref ss (1+ k)) curr))
                               res)))
        finally (return res)))


(defun mid-rad (pts)
  (loop with mid = (vec:lmid (to-list (subseq pts 0 (1- (length pts)))))
        for p across pts
        maximize (vec:dst mid p) into rad
        finally (return (values mid rad))))


(defun -get-lines (mid dst a n &key (offset-fx #'vec:add))
  (loop with slide = (vec:smult (vec:cos-sin (- a (* 0.5 PI))) dst)
        with offset = (vec:smult (vec:cos-sin a) dst)
        with res = (make-adjustable-vector)
        for s in (math:linspace n 0d0 1d0)
        do (let ((xy (vec:on-line s mid (funcall offset-fx mid offset))))
             (vextend (list (vec:add xy slide) (vec:sub xy slide))
                      res))
        finally (return res)))


(defun -line-hatch (res line pts)
  (let ((ixs (make-adjustable-vector)))

    (loop for i from 0 below (1- (length pts))
          do (multiple-value-bind (x s)
               (vec:segx line (list (aref pts i) (aref pts (1+ i))))
               (if x (vextend s ixs))))

    (setf ixs (sort ixs #'<))

    (loop for i from 0 below (1- (length ixs)) by 2
          do (vextend (list (vec:lon-line (aref ixs i) line)
                            (vec:lon-line (aref ixs (1+ i)) line))
                      res))

    res))


(defun hatch (pts &key (angles 0d0) (rs 1d0)
                  &aux (pts* (ensure-vector pts)))
  "
  draw hatches at angles inside the area enclosed by the path in pts
  "
  (when (> (vec:dst (aref pts* 0) (vector-last pts*)) 0.0001d0)
        (error "first and last element in pts must be close to each other."))
  (multiple-value-bind (mid dst) (mid-rad pts*)
    (loop with res = (make-adjustable-vector)
          with n = (ceiling (* 0.5d0 rs dst))
          for a in (if (equal (type-of angles) 'cons) angles (list angles))
          do (loop for line across (-get-lines mid dst a n)
                   do (-line-hatch res line pts*))
             (loop for line across
                     (subseq (-get-lines mid dst a n :offset-fx #'vec:sub) 1)
                   do (-line-hatch res line pts*))
          ; TODO: probably need to filter out empty lines in result
          finally (return res))))

