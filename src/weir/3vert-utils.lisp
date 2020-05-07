(in-package :weir)

(declaim (inline -3dimtest))
(defun -3dimtest (wer)
  (declare #.*opt-settings* (weir wer))
  (when (not (= (weir-dim wer) 3)) (error "wrong dimension, use 3.")))

(defun 3get-verts (wer vv &aux (vv* (if (equal (type-of vv) 'cons)
                                       vv (to-list vv))))
  "
  get the coordinates (vec) of verts in vv
  "
  (declare #.*opt-settings* (weir wer) (sequence vv))
  (-3dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (-valid-verts (num-verts vv* v)
      (avec:3getv verts v))))


(defun 3get-grp-verts (wer &key g order)
  "
  returns all vertices in grp g.
  note: verts only belong to a grp if they are part of an edge in grp.
  "
  (declare #.*opt-settings* (weir wer) (boolean order))
  (-3dimtest wer)
  (3get-verts wer (get-vert-inds wer :g g :order order)))


(defun 3add-vert! (wer xy)
  "
  adds a new vertex to weir
  returns the new vert ind.
  "
  (declare #.*opt-settings* (weir wer) (vec:3vec xy))
  (-3dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (avec:3setv verts num-verts xy))
  (1- (incf (weir-num-verts wer))))


(defun 3add-verts! (wer vv)
  "
  adds new vertices to weir
  returns the ids of the new vertices
  "
  (declare #.*opt-settings* (weir wer) (list vv))
  (-3dimtest wer)
  (loop for xy of-type vec:3vec in vv
        collect (3add-vert! wer xy)))


(defun 3get-vert (wer v)
  "
  get the coordinate (vec) of vert v.
  "
  (declare #.*opt-settings* (weir wer) (pos-int v))
  (-3dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (-valid-vert (num-verts v)
      (avec:3getv verts v))))


(defun 3get-all-verts (wer)
  "
  returns the coordinates (vec) of all vertices.
  "
  (declare #.*opt-settings* (weir wer))
  (-3dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (loop for v of-type pos-int from 0 below num-verts
          collect (avec:3getv verts v) of-type vec:3vec)))


(defun 3make-vert-getter (wer)
  (declare (weir wer))
  (let ((verts (to-vector (weir:3get-all-verts wer))))
    (declare (type simple-array verts))
    (lambda (vv) (declare (list vv))
      (mapcar (lambda (i) (declare (pos-int i))
                (aref verts i)) vv))))


(declaim (inline 3move-vert!))
(defun 3move-vert! (wer i v &key (rel t) (ret nil))
  (declare #.*opt-settings*
           (weir wer) (pos-int i) (vec:3vec v) (boolean rel))
  (-3dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (when (>= i num-verts)
          (error "attempting to move invalid vert, ~a (~a)" i num-verts))
    (vec:3with-xy (v vx vy vz)
      (avec:3with-vec (verts i x y z)
        (if rel (setf x (+ x vx) y (+ y vy) z (+ z vz))
                (setf x vx y vy z vz))
        (when ret (vec:3vec x y z))))))


(defun 3transform! (wer inds fx)
  (declare (weir wer) (function fx) (list inds))
  (-3dimtest wer)
  (let* ((verts (3get-verts wer inds)))
    (declare (list verts))
    (loop for i of-type pos-int in inds
          and p of-type vec:3vec in (funcall fx verts)
          do (3move-vert! wer i p :rel nil))))

(defun 3grp-transform! (wer fx &key g)
  (declare (weir wer) (function fx))
  (3transform! wer (get-vert-inds wer :g g) fx))


(defun 3edge-length (wer a b)
  "
  returns the length of edge (a b).
  "
  (declare #.*opt-settings* (weir wer) (pos-int a b))
  (-3dimtest wer)
  (with-struct (weir- verts) wer
    (declare (type (simple-array double-float) verts))
    (avec:3dst verts verts a b)))


(defun 3ledge-length (wer e)
  "
  returns the length of edge e=(a b).
  "
  (declare #.*opt-settings* (weir wer) (list e))
  (-3dimtest wer)
  (apply #'3edge-length wer e))


(defun 3split-edge! (wer u v &key xy g)
  "
  split edge at xy (or middle if xy is nil).
  returns new vert ind (and new edges).
  "
  (declare #.*opt-settings* (weir wer) (pos-int u v)
                            (vec:3vec xy))
  (-3dimtest wer)
  (del-edge! wer u v :g g)
  (let ((c (3add-vert! wer xy)))
    (declare (pos-int c))
    (let ((edges (list (add-edge! wer c u :g g)
                       (add-edge! wer c v :g g))))
      (declare (list edges))
      (values c edges))))

(defun 3lsplit-edge! (wer ll &key xy g)
  (declare #.*opt-settings* (weir wer) (list ll) (vec:3vec xy))
  (-3dimtest wer)
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (3split-edge! wer a b :xy xy :g g)))


(defun 3prune-edges-by-len! (wer lim &optional (fx #'>))
  "
  remove edges longer than lim, use fx #'< to remove edges shorter than
  lim.
  "
  (declare #.*opt-settings* (weir wer) (double-float lim) (function fx))
  (-3dimtest wer)
  (itr-edges (wer e)
    (when (funcall (the function fx) (3ledge-length wer e) lim)
          (ldel-edge! wer e))))


(declaim (inline -3center))
(defun -3center (verts v mid mx my mz &key (s 1d0))
  (declare #.*opt-settings* (type (simple-array double-float) verts)
           (pos-int v) (vec:vec mid) (double-float mx my my s))
  (avec:3with-vec (verts v x y z)
    (setf x (+ (vec:3vec-x mid) (* s (- x mx)))
          y (+ (vec:3vec-y mid) (* s (- y my)))
          z (+ (vec:3vec-z mid) (* s (- z mz))))))

(declaim (inline -scale-by))
(defun -3scale-by (max-side sx sy sz)
  (declare #.*opt-settings* (double-float sx sy sz))
  (cond ((not max-side) 1d0)
        ((and (> sx sy) (> sx sz)) (/ (the double-float max-side) sx))
        ((and (> sy sx) (> sy sz)) (/ (the double-float max-side) sy))
        (t (/ (the double-float max-side) sz))))

(defun 3center! (wer &key (xy vec:*3zero*) max-side (non-edge t))
  "
  center the verts of wer on xy. returns the previous center.
  "
  (-3dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (multiple-value-bind (minx maxx miny maxy minz maxz)
      (if non-edge (avec:3minmax verts num-verts)
                   (avec::3minmax* verts (get-vert-inds wer :g g)))
      (let ((mx (* 0.5d0 (+ minx maxx)))
            (my (* 0.5d0 (+ miny maxy)))
            (mz (* 0.5d0 (+ minz maxz)))
            (w (- maxx minx))
            (h (- maxy miny))
            (d (- maxz minz)   )
            (s (-3scale-by max-side w h d)))
        (declare (double-float mx my mz))
        (itr-verts (wer v) (-3center verts v xy mx my mz :s s))
        (values (vec:3vec mx my mz) (vec:3vec w h d) s)))))


(defun 3add-path! (wer points &key g closed)
  (declare #.*opt-settings* (weir wer) (list points))
  (-3dimtest wer)
  (add-path-ind! wer (3add-verts! wer points)
                 :g g :closed closed))


(defun 3add-box! (wer &key (sx 1d0) (sy 1d0) (sz 1d0) (xy vec:*3zero*) g)
  (declare (weir wer) (double-float sx sy sz) (vec:3vec xy))
  (-3dimtest wer)
  (let* ((verts (make-hash-table :test #'equal))
         (gp (lambda (l) (declare (list l)) (loop for v of-type list in l
                                                  collect (gethash v verts))))
         (addvert (lambda (xyzi xyz) (declare (list xyzi) (vec:3vec xyz))
                    (setf (gethash xyzi verts)
                          (3add-vert! wer (vec:3add xy xyz))))))

    (loop for z of-type double-float in (list (- sz) sz)
          and zi of-type pos-int from 0
          do (loop for x of-type double-float in (list (- sx) sx)
                   and xi of-type pos-int from 0
                   do (loop for y of-type double-float in (list (- sy) sy)
                            and yi of-type pos-int from 0
                            do (funcall addvert (list xi yi zi)
                                                (vec:3vec x y z)))))

    (add-path-ind! wer (funcall gp '((0 0 0) (0 1 0) (1 1 0) (1 0 0)))
                   :closed t :g g)
    (add-path-ind! wer (funcall gp '((0 0 1) (0 1 1) (1 1 1) (1 0 1)))
                   :closed t :g g)
    (ladd-edge! wer (funcall gp '((0 0 0) (0 0 1))) :g g)
    (ladd-edge! wer (funcall gp '((1 0 0) (1 0 1))) :g g)
    (ladd-edge! wer (funcall gp '((0 1 0) (0 1 1))) :g g)
    (ladd-edge! wer (funcall gp '((1 1 0) (1 1 1))) :g g)))


(defun 3add-cube! (wer &key (s 1d0) (xy vec:*3zero*) g)
  (declare (weir wer) (double-float s) (vec:3vec xy))
  (3add-box! wer :sx s :sy s :sz s :xy xy :g g))


(defun 3build-kdtree (wer)
  (declare (weir wer))
  (-3dimtest wer)
  (setf (weir-kd wer) (kdtree:make* (weir-verts wer)
                                    (weir-num-verts wer))))


(declaim (inline -3is-rel-neigh))
(defun -3is-rel-neigh (verts u v near)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts)
           (pos-int u v)
           (list near))
  (loop with d of-type double-float = (avec:3dst2 verts verts u v)
        for w of-type pos-int in near
        if (not (> (the double-float
                        (max (the double-float (avec:3dst2 verts verts u w))
                             (the double-float (avec:3dst2 verts verts v w)))) d))
          summing 1 into c of-type pos-int
        ; TODO: avoid this by stripping u from near
        if (> c 1) do (return-from -3is-rel-neigh nil))
  t)

(defun 3relative-neighborhood! (wer rad &key g)
  "
  find the relative neigborhood graph (limited by the radius rad) of verts
  in wer. the graph is made in grp g.
  "
  (declare #.*opt-settings* (weir wer) (double-float rad))
  (-3dimtest wer)
  (3build-kdtree wer)
  (let ((c 0))
    (declare (pos-int c))
    (itr-verts (wer v)
      (loop with verts of-type (simple-array double-float) = (weir-verts wer)
            with near of-type list =
                (to-list (remove-if (lambda (x)
                                      (declare (pos-int x))
                                      (= x v))
                                    (3verts-in-rad wer (3get-vert wer v) rad)))
            ; TODO: strip u from near
            for u of-type pos-int in near
            if (and (< u v) (-3is-rel-neigh verts u v near))
            do (when (add-edge! wer u v :g g) (incf c))))
    c))


(defun 3verts-in-rad (wer xy rad)
  (declare #.*opt-settings* (weir wer) (vec:3vec xy) (double-float rad))
  (-3dimtest wer)
  (kdtree:rad (weir-kd wer) xy rad))


(defun 3export-verts-grp (wer &key g)
  "
  export verts, as well as the edges in g, on the format (verts edges)
  "
  (declare #.*opt-settings* (weir wer))
  (-3dimtest wer)
  (list (3get-all-verts wer) (get-edges wer :g g)))

(defun 3import-verts-grp (wer o &key g)
  "
  import data exported using 3export-verts-grp.
  "
  (declare #.*opt-settings* (weir wer) (list o))
  (-3dimtest wer)
  (when (> (the pos-int (get-num-verts wer)) 0)
        (error "ensure there are no initial verts in wer."))
  (destructuring-bind (verts edges) o
    (declare (list verts edges))
    (3add-verts! wer verts)
    (loop for e of-type list in edges do (ladd-edge! wer e :g g))))

