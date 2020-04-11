
(in-package :kdtree)


(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))


(declaim (inline -make-node))
(defstruct (node (:constructor -make-node))
  (l nil :read-only t)
  (r nil :read-only t)
  (d 0 :type pos-int :read-only t)
  (m 0 :type pos-int :read-only t)
  (ax 0 :type pos-int :read-only t))


(declaim (inline -make-kdtree))
(defstruct (kdtree (:constructor -make-kdtree))
  (verts nil :type (simple-array double-float) :read-only t)
  (root nil :type node :read-only t)
  (n nil :type pos-int :read-only t))


(declaim (inline -inds-container))
(defun -inds-container (n)
  (declare #.*opt-settings* (pos-int n))
  (loop repeat 3 collect (make-array n :element-type 'pos-int)))


(declaim (inline -median-split))
(defun -median-split (verts n ax inds)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts)
           (pos-int n ax) (list inds))
  (let* ((mv (aref (the (simple-array pos-int) (nth ax inds))
                   (the pos-int (floor n 2))))
         (fl (floor n 2))
         (ll (-inds-container fl))
         (rr (-inds-container (- n 1 fl)))
         (mx (aref verts (+ ax mv))))
    (declare (pos-int fl) (list ll rr) (double-float mx))
    (loop for ind of-type (simple-array pos-int) in inds
          and l of-type (simple-array pos-int) in ll
          and r of-type (simple-array pos-int) in rr
          do (loop with li of-type pos-int = 0
                   with ri of-type pos-int = 0
                   for i of-type pos-int across ind
                   do (when (not (= i mv))
                        (if (<= (aref verts (+ ax i)) mx)
                            (progn (setf (aref l li) i) (incf li))
                            (progn (setf (aref r ri) i) (incf ri))))))
    (values mv ll rr)))



(defun -tree (verts d inds)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts)
           (pos-int d) (list inds))
  (let ((n (length (first inds)))
        (ax (mod d 3))
        (dd (1+ d)))
    (declare (pos-int ax dd n))
    (when (< n 1) (return-from -tree))
    (when (< n 2) (return-from -tree
                    (-make-node :m (aref (first inds) 0) :d d :ax ax)))
    (multiple-value-bind (mv ll rr) (-median-split verts n ax inds)
      (declare (pos-int mv) (list ll rr))
      (-make-node :m mv :d d :ax ax :l (-tree verts dd ll)
                                    :r (-tree verts dd rr)))))


(declaim (inline -set-verts))
(defun -set-verts (nn vv)
  (declare #.*opt-settings* (pos-int nn) (list vv))
  (loop with verts of-type (simple-array double-float) =
          (make-array nn :element-type 'double-float)
        for v of-type vec:3vec in vv
        and i of-type pos-int from 0
        do (avec:3setv verts i v)
        finally (return verts)))


(declaim (inline -get-inds))
(defun -get-inds (verts nn)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts)
           (pos-int nn))
  (loop for ax of-type pos-int from 0 below 3
        collect (to-vector
                  (sort (the list
                             (loop for i of-type pos-int from 0 below nn by 3
                                   collect i))
                        #'< :key (lambda (i) (declare (pos-int i))
                                   (aref verts (+ i ax))))
             :type 'pos-int)))


(defun make (vv)
  (declare #.*opt-settings* (list vv))
  (let* ((n (length vv))
         (nn (* 3 n))
         (verts (-set-verts nn vv)))
    (declare (type (simple-array double-float) verts) (pos-int n nn))
    (-make-kdtree :verts verts
                  :root (-tree verts 0 (-get-inds verts nn))
                  :n n)))


(defun make* (verts &optional n)
  (declare #.*opt-settings* (type (simple-array double-float) verts))
  (let* ((n* (if n n (/ (length verts) 3)))
         (nn (* 3 n*)))
    (declare (pos-int n* nn))
    (-make-kdtree :verts verts
                  :root (-tree verts 0 (-get-inds verts nn))
                  :n n*)))


(declaim (inline -dst2))
(defun -dst2 (verts i x y z)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts) (pos-int i)
           (double-float x y z))
  (+ (expt (- (the double-float (aref verts i)) x) 2d0)
     (expt (- (the double-float (aref verts (+ 1 i))) y) 2d0)
     (expt (- (the double-float (aref verts (+ 2 i))) z) 2d0)))


(declaim (inline -make-nn-res nn-res-m nn-res-d))
(defstruct (nn-res (:constructor -make-nn-res))
  (m nil :type pos-int :read-only nil)
  (d nil :type double-float :read-only nil))


(defun -nn (verts nod x y z &key best)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts)
           (double-float x y z))
  (when (not nod) (return-from -nn))
  (with-struct (node- m l r ax) nod
    (declare (pos-int m ax))
    (let* ((ptaxv (case ax (0 x) (1 y) (t z)))
           (nodeaxv (aref verts (+ m ax)))
           (axdst (expt (- ptaxv nodeaxv) 2d0))
           (dst (-dst2 verts m x y z)))
      (declare (pos-int ax) (double-float axdst dst nodeaxv ptaxv))
      (when (< dst (nn-res-d best))
            (setf (nn-res-m best) (node-m nod) (nn-res-d best) dst))
      (if (> (nn-res-d best) axdst)
          (progn (-nn verts r x y z :best best)
                 (-nn verts l x y z :best best))
          (-nn verts (if (<= ptaxv nodeaxv) l r) x y z :best best)))))


(defun nn (kd xy)
  (declare #.*opt-settings* (kdtree kd))
  (with-struct (kdtree- root verts) kd
    (declare (type (simple-array double-float) verts))
    (let* ((x (vec:3vec-x xy))
           (y (vec:3vec-y xy))
           (z (vec:3vec-z xy))
           (m (node-m root))
           (best (-make-nn-res :m m :d (-dst2 verts m x y z))))
      (-nn verts root x y z :best best)
      (/ (nn-res-m best) 3))))


(defun -rad (verts nod x y z rad2 &key res)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts)
           (type (vector pos-int) res)
           (double-float x y z rad2))
  (when (not nod) (return-from -rad))
  (with-struct (node- m l r ax) nod
    (declare (pos-int m ax))
    (let* ((ptaxv (case ax (0 x) (1 y) (t z)))
           (nodeaxv (aref verts (+ m ax)))
           (axdst (expt (- ptaxv nodeaxv) 2d0))
           (dst (-dst2 verts m x y z)))
      (declare (pos-int ax) (double-float axdst dst nodeaxv ptaxv))
      (when (< dst rad2) (vextend (/ m 3) res))
      (if (> rad2 axdst)
          (progn (-rad verts r x y z rad2 :res res)
                 (-rad verts l x y z rad2 :res res))
          (-rad verts (if (<= ptaxv nodeaxv) l r) x y z rad2 :res res)))))


(defun rad (kd xy rad)
  (declare #.*opt-settings* (kdtree kd) (double-float rad))
  (with-struct (kdtree- root verts) kd
    (declare (type (simple-array double-float) verts))
    (let* ((x (vec:3vec-x xy))
           (y (vec:3vec-y xy))
           (z (vec:3vec-z xy))
           (res (make-adjustable-vector :type 'pos-int)))
      (-rad verts root x y z (expt rad 2d0) :res res)
      res)))

