
(in-package :math)


(defun line-from (a r &optional (s 1d0))
  (declare (vec:vec a r) (double-float s))
  (list a (vec:from a r s)))


(defun path-tangents (aa &key closed (default vec:*zero*)
                         &aux (aa* (if (equal (type-of aa) 'cons)
                                       (make-adjustable-vector :init aa :type 'vec:vec)
                                       aa)))
  (when closed (vextend (aref aa* 0) aa*))
  (loop with res = (make-adjustable-vector :type 'vec:vec)
        for i from 0 below (1- (length aa*))
        do (vextend (vec:nsub (aref aa* (1+ i)) (aref aa* i)
                                 :default default)
                    res)
        finally (return res)))


(defun path-angles (pts)
  (loop with n of-type pos-int = (1- (length pts))
        with res = (make-array (1+ n) :element-type 'vec:vec
                                      :adjustable nil
                                      :initial-element vec:*zero*)
        for i of-type pos-int from 0 below n
        do (setf (aref res i) (vec:norm! (vec:sub (aref pts (1+ i))
                                                  (aref pts i))))
        finally (setf (aref res n) (aref res (1- n)))
                (return res)))


; TODO: closed?
(defun path-length (path)
  (declare (list path))
  (loop for a of-type vec:vec in path
        and b of-type vec:vec in (cdr path)
        summing (vec:dst a b)))

(defun 3path-length (path)
  (declare (list path))
  (loop for a of-type vec:3vec in path
        and b of-type vec:3vec in (cdr path)
        summing (vec:3dst a b)))


; ----- STIPPLE -----

; more or less as suggested in
; https://gist.github.com/evanmiltenburg/dfd571f27372477487cb14f2bdf8b35c

(defun -stipple-get-lengths (num-lines len)
  (loop with lens of-type list = (rnd:nrnd num-lines)
        with s of-type double-float = (/ (dsum lens))
        for l in lens collect (* (* l len) s)))

(defun stipple (num-lines len)
  "
  draw num-lines stipples between (0 1) the stipples will have a total length
  of len
  "
  (declare #.*opt-settings* (pos-int num-lines) (double-float len))
  (let ((lengths (-stipple-get-lengths num-lines len))
        (gaps (-stipple-get-lengths (1- num-lines) (- 1d0 len))))
    (declare (list lengths gaps))
    (loop with curr of-type double-float = (first lengths)
          with res of-type vector = (to-adjustable-vector
                                      (list (list 0d0 curr)) :type 'vec:vec)
          for l of-type double-float in (cdr lengths)
          and g of-type double-float in gaps
          do (vextend (list curr (+ curr l)) res)
             (incf curr (+ l g))
          finally (return res))))

