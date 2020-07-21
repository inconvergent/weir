(in-package :mesh)


(defun get-verts (msh vv &aux (vv* (if (equal (type-of vv) 'cons)
                                       vv (to-list vv))))
  "
  get the coordinates (vec) of verts in vv
  "
  (declare #.*opt-settings* (mesh msh) (sequence vv))
  (with-struct (mesh- verts num-verts) msh
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (-valid-verts (num-verts vv* v)
      (avec:3getv verts v))))


(defun add-vert! (msh xy)
  "
  adds a new vertex to mesh
  returns the new vert ind.
  "
  (declare #.*opt-settings* (mesh msh) (vec:3vec xy))
  (with-struct (mesh- verts num-verts) msh
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (avec:3setv verts num-verts xy))
  (1- (incf (mesh-num-verts msh))))


(defun add-verts! (msh vv)
  "
  adds new vertices to mesh
  returns the ids of the new vertices
  "
  (declare #.*opt-settings* (mesh msh) (list vv))
  (loop for xy of-type vec:3vec in vv collect (add-vert! msh xy) of-type pos-int))


(defun get-vert (msh v)
  "
  get the coordinate (vec) of vert v.
  "
  (declare #.*opt-settings* (mesh msh) (pos-int v))
  (with-struct (mesh- verts num-verts) msh
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (-valid-vert (num-verts v)
      (avec:3getv verts v))))


(defun get-all-verts (msh)
  "
  returns the coordinates (vec) of all vertices.
  "
  (declare #.*opt-settings* (mesh msh))
  (with-struct (mesh- verts num-verts) msh
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (loop for v of-type pos-int from 0 below num-verts
          collect (avec:3getv verts v) of-type vec:3vec)))


(declaim (inline move-vert!))
(defun move-vert! (msh i v &key (rel t) (ret nil))
  (declare #.*opt-settings*
           (mesh msh) (pos-int i) (vec:3vec v) (boolean rel))
  (with-struct (mesh- verts num-verts) msh
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (when (>= i num-verts)
          (error "attempting to move invalid vert, ~a (~a)" i num-verts))
    (vec:3with-xy (v vx vy vz)
      (avec:3with-vec (verts i x y z)
        (if rel (setf x (+ x vx) y (+ y vy) z (+ z vz))
                (setf x vx y vy z vz))
        (when ret (vec:3vec x y z))))))


(defun transform! (msh inds fx)
  (declare (mesh msh) (function fx) (list inds))
  (let* ((verts (get-verts msh inds)))
    (declare (list verts))
    (loop for i of-type pos-int in inds
          and p of-type vec:3vec in (funcall fx verts)
          do (move-vert! msh i p :rel nil))))

(defun transform*! (msh inds fx)
  (declare (mesh msh) (list inds) (function fx))
  (transform! msh (remove-duplicates (alexandria:flatten inds)) fx))


(defun make-vert-getter (msh)
  (declare (mesh msh))
  (let ((verts (to-vector (get-all-verts msh))))
    (declare (type simple-array verts))
    (lambda (vv) (declare (list vv))
      (mapcar (lambda (i) (declare (pos-int i))
                          (aref verts i)) vv))))


(defun edge-length (msh e)
  "
  returns the length of edge e=(a b).
  "
  (declare #.*opt-settings* (mesh msh) (list e))
  (with-struct (mesh- verts) msh
    (declare (type (simple-array double-float) verts))
    (apply #'avec:3dst verts verts e)))


(declaim (inline -center))
(defun -center (verts v xy mx my mz)
  (avec:3with-vec (verts v x y z)
    (setf x (+ (vec:3vec-x xy) (- x mx))
          y (+ (vec:3vec-y xy) (- y my))
          z (+ (vec:3vec-z xy) (- z mz)))))

(defun center! (msh &key (xy vec:*3zero*))
  "
  center the verts of msh on xy. returns the previous center.
  "
  (with-struct (mesh- verts num-verts) msh
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (multiple-value-bind (minx maxx miny maxy minz maxz)
      (avec:3minmax verts num-verts)
      (let ((mx (* 0.5d0 (+ minx maxx)))
            (my (* 0.5d0 (+ miny maxy)))
            (mz (* 0.5d0 (+ minz maxz))))
        (declare (double-float mx my mz))
        (loop for v from 0 below num-verts
              do (-center verts v xy mx my mz))
        (vec:3vec mx my mz)))))


(declaim (inline -poly-normal))
(defun -poly-normal (a b c)
  (declare  #.*opt-settings* (vec:3vec a b c))
  ; (vec:3norm (vec:3cross! (vec:3sub b a) (vec:3sub c a)))
  (multiple-value-bind (x y z)
    (math:3cross (- (vec:3vec-x b) (vec:3vec-x a))
                 (- (vec:3vec-y b) (vec:3vec-y a))
                 (- (vec:3vec-z b) (vec:3vec-z a))
                 (- (vec:3vec-x c) (vec:3vec-x a))
                 (- (vec:3vec-y c) (vec:3vec-y a))
                 (- (vec:3vec-z c) (vec:3vec-z a)))
    (declare (double-float x y z))
    (let ((l2 (+ (* x x) (* y y) (* z z))))
      (declare (double-float l2))
      (if (> l2 0d0) (let ((l (sqrt (the pos-double l2))))
                       (declare (double-float l))
                       (vec:3vec (/ x l) (/ y l) (/ z l)))
                     (vec:3zero)))))

(defun normal (msh poly)
  (declare #.*opt-settings* (mesh msh) (list poly))
  (destructuring-bind (a b c) (get-verts msh poly)
    (declare (vec:3vec a b c))
    (-poly-normal a b c)))


(defun polyx (msh poly line)
  (declare #.*opt-settings* (mesh msh) (list poly line))
  (vec:3polyx (get-verts msh poly) line))


(defun -do-extrude (msh polya polyb)
  (declare #.*opt-settings* (mesh msh) (list polya polyb))
  (destructuring-bind (a1 b1 c1) polya
    (destructuring-bind (a2 b2 c2) polyb
      (add-polygons! msh (list (list a1 b1 a2) (list a2 b2 b1)
                               (list b1 c1 b2) (list b2 c2 c1)
                               (list c1 a1 c2) (list c2 a2 a1))))))

(defun extrude-polygon! (msh poly dir)
  (declare #.*opt-settings* (mesh msh) (list poly) (vec:3vec dir))
  (if (del-polygon! msh poly)
      (let* ((polyb (loop for v in (get-verts msh poly)
                          collect (add-vert! msh (vec:3add v dir))))
             (sides (-do-extrude msh poly polyb)))
        (values polyb sides))
      (values nil nil)))

