
(in-package :simplify-path)

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))

(deftype int-vector () `(vector pos-int))

(deftype vec-simple () `(simple-array vec:vec))


;note: it would be better if we could avoid using an adjustable vector.
(defun -simplify (pts lim &key left right)
  (declare #.*opt-settings* (double-float lim)
                            (vec-simple pts) (pos-int left right))
  (let ((res (make-adjustable-vector :type 'pos-int))
        (dmax -1d0)
        (index 0))
    (declare (int-vector res) (pos-int index) (double-float dmax))

    (loop with seg of-type list = (list (aref pts left) (aref pts right))
          for i of-type pos-int from (1+ left) below right
          do (let ((d (vec:segdst seg (aref pts i))))
               (declare (double-float d))
               (when (> d dmax) (setf dmax d index i))))

    (if (> dmax lim)
        (progn (loop with ps of-type int-vector =
                       (-simplify pts lim :left left :right index)
                     for i from 0 below (1- (length ps))
                     do (vextend (aref ps i) res))
               (loop for i across (-simplify pts lim :left index :right right)
                     do (vextend i res)))
        (progn (vextend left res)
               (vextend right res)))
    (sort res #'<)))


; https://hydra.hull.ac.uk/resources/hull:8338
(defun simplify (pts &key (lim 1d0))
  (declare #.*opt-settings* (vec-simple pts) (double-float lim))
  (loop for i of-type pos-int across
          (-simplify pts lim :left 0 :right (1- (length pts)))
        collect (aref pts i) of-type vec:vec))

