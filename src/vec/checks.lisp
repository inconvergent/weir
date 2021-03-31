
(in-package :vec)


(declaim (inline ptinside))
(defun ptinside (convex v)
  (declare #.*opt-settings* (list convex) (vec v))
  (loop with convex* = (math:close-path* convex)
        for a of-type vec in convex*
        and b of-type vec in (cdr convex*)
        always (>= (cross (sub b a) (sub v b)) 0d0)))


(declaim (inline segdst))
(defun segdst (line v)
  "
  find distance between line and v.
  returns values (distance s) where is is the interpolation value that will
  yield the closest point on line.
  "
  (declare #.*opt-settings* (list line) (vec v))
  (destructuring-bind (va vb) line
    (declare (vec va vb))
    (let ((l2 (dst2 va vb)))
      (declare (double-float l2))
      (if (<= l2 0d0)
        ; line is a point
        (values (dst va v) 0d0)
        ; else
        (let ((tt (/ (+ (* (- (vec-x v) (vec-x va)) (- (vec-x vb) (vec-x va)))
                        (* (- (vec-y v) (vec-y va)) (- (vec-y vb) (vec-y va))))
                     l2)))
          (if (> tt 1d0) (setf tt 1d0))
          (if (< tt 0d0) (setf tt 0d0))
          (values (dst v (on-line tt va vb)) tt))))))


; TODO: this is slowish
(declaim (inline segx))
(defun segx (aa bb)
  (declare #.*opt-settings* (list aa bb))
  "
  find intersection between lines aa, bb.
  returns isect? p q where p and q is the distance along each line to the
  intersection point
  "
  (destructuring-bind (a1 a2) aa
    (declare (vec a1 a2))
    (destructuring-bind (b1 b2) bb
      (declare (vec b1 b2))
      (let* ((sa (sub a2 a1))
             (sb (sub b2 b1))
             (u (cross sa sb)))
        (declare (vec sa sb) (double-float u))
        (if (<= (abs u) 0d0)
          ; return nil if the lines are parallel (nil)
          ; this is just a div0 guard. it's not a good way to test.
          (values nil 0d0 0d0)
          ; otherwise check if they intersect
          (let* ((ab (sub a1 b1))
                 (p (/ (cross sa ab) u))
                 (q (/ (cross sb ab) u)))
            (declare (vec ab) (double-float p q))
            ; t if intersection, nil otherwise
            (values (and (> p 0d0) (< p 1d0) (> q 0d0) (< q 1d0))
                    q p)))))))


(deftype array-list () `(simple-array list))

(declaim (inline -sweep-line))
(defun -sweep-line (lines line-points)
  (declare #.*opt-settings* (array-list lines) (list line-points))
  "perform sweep line search for intersections along x"
  ; add first line index to sweep line state,
  ; and set sweep line position
  ; TODO: special cases: equal x pos, vertical line
  (let ((res (make-array (length lines) :element-type 'list
                                        :initial-element nil
                                        :adjustable nil))
        (q (hset:make :init (list (cdar line-points)))))
    (declare (type (simple-array list) res) (hash-table q))

    (labels
      ((-append (i c p)
         (declare (fixnum i c) (double-float p))
         (if (aref res i) (push `(,c . ,p) (aref res i))
                          (setf (aref res i) `((,c . ,p)))))

       (-isects (i cands)
         (declare (fixnum i) (list cands))
         "intersection test"
         ; TODO: avoid calling segx, i think this can be improved.
         (loop with line of-type list = (aref lines i)
               for c of-type fixnum in cands
               do (multiple-value-bind (x p qq) (segx line (aref lines c))
                    (when x (-append i c p)
                            (-append c i qq))))))

      (loop for (_ . i) of-type (double-float . fixnum) in (cdr line-points)
            ; if i in q, kick i out of q,
            if (hset:mem q i) do (hset:del q i)
            ; else check i against all q, add i to q
            else do (-isects i (hset:to-list q))
                    (hset:add q i)))
    res))

(declaim (inline -sorted-point-pairs))
(defun -sorted-point-pairs (lines)
  (declare #.*opt-settings* (array-list lines))
  (loop with res of-type list = (list)
        for (a b) of-type (vec vec) across lines
        for i of-type fixnum from 0
        do (push `(,(vec-x a) . ,i) res)
           (push `(,(vec-x b) . ,i) res)
        finally (return (sort res #'< :key #'car))))

(defun lsegx (lines*)
  (declare (sequence lines*))
  "
  not entirely slow line-line intersection for all lines. this is faster than
  comparing all lines when lines are short relative to the area that the lines
  cover. it can be improved further by using binary search tree to store
  current state.
  "
  (let ((lines (if (listp lines*) (weir-utils:to-vector lines* :type 'list)
                                  lines*)))
    (declare (array-list lines))
    (-sweep-line lines (-sorted-point-pairs lines))))

