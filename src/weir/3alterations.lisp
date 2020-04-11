(in-package :weir)

(declaim (inline 3add-vert?))
(defun 3add-vert? (xy)
  "add new vert at xy. returns the new vert ind"
  (declare (vec:3vec xy))
  (lambda (wer) (3add-vert! wer xy)))


(declaim (inline 3vadd-edge?))
(defun 3vadd-edge? (xya xyb &key g)
  "add verts xya and xyb, and creates an edge (in grp g) between them"
  (declare (vec:3vec xya xyb))
  (lambda (wer)
    (add-edge! wer (add-vert! wer xya) (add-vert! wer xyb) :g g)))


(declaim (inline 3move-vert?))
(defun 3move-vert? (v xy &key (rel t))
  "
  move vert v.
    if rel: move relative to original position.
    else: move to xy.
  "
  (declare (pos-int v) (vec:3vec xy) (boolean rel))
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (3move-vert! wer v xy :rel rel :ret t))))


(declaim (inline 3append-edge?))
(defun 3append-edge? (v xy &key (rel t) g)
  "add edge between vert v and new vert xy"
  (declare (pos-int v) (vec:3vec xy) (boolean rel))
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (let ((w (if rel (3add-vert! wer (vec:3add (3get-vert wer v) xy))
                       (3add-vert! wer xy))))
        (declare (pos-int w))
        (add-edge! wer v w :g g)
        w))))


(declaim (inline 3add-edge?))
(defun 3add-edge? (v w &key g)
  "create edge between valid verts v and w (in grp g)"
  (declare (pos-int v w))
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (-valid-vert ((weir-num-verts wer) w :err nil)
        (ladd-edge! wer (list v w) :g g)))))

(declaim (inline ladd-edge?))
(defun 3ladd-edge? (ll &key g)
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (add-edge? a b :g g)))


(declaim (inline 3del-edge?))
(defun 3del-edge? (v w &key g)
  "del edge (v w) (of grp g)"
  (declare (pos-int v w))
  (lambda (wer)
    (del-edge! wer v w :g g)))

(declaim (inline ldel-edge?))
(defun 3ldel-edge? (ll &key g)
  (declare (list ll))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (del-edge? a b :g g)))


(declaim (inline 3split-edge?))
(defun 3split-edge? (v w &key xy g)
  "
  insert a vert, v, on edge e = (v w) such that we get edges (a v) and (v b).
  v will be positioned at xy. returns the new edges (or nil).
  "
  (declare (pos-int v w))
  (lambda (wer)
    (when (edge-exists wer (list v w) :g g)
          (3split-edge! wer v w :xy xy :g g))))

(declaim (inline lsplit-edge?))
(defun 3lsplit-edge? (ll &key xy g)
  (declare (list ll))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (3split-edge? a b :xy xy :g g)))

