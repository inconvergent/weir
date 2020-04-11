
(in-package :weir)

(declaim (inline add-vert?))
(defun add-vert? (xy)
  "add new vert at xy. returns the new vert ind"
  (declare (vec:vec xy))
  (lambda (wer) (add-vert! wer xy)))


(declaim (inline vadd-edge?))
(defun vadd-edge? (xya xyb &key g)
  "add verts xya and xyb, and creates an edge (in grp g) between them"
  (declare (vec:vec xya xyb))
  (lambda (wer) (add-edge! wer (add-vert! wer xya) (add-vert! wer xyb) :g g)))


(declaim (inline move-vert?))
(defun move-vert? (v xy &key (rel t))
  "
  move vert v.
    if rel: move relative to original position.
    else: move to xy.
  "
  (declare (pos-int v) (vec:vec xy) (boolean rel))
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (move-vert! wer v xy :rel rel :ret t))))


(declaim (inline append-edge?))
(defun append-edge? (v xy &key (rel t) g)
  "add edge between vert v and new vert xy"
  (declare (pos-int v) (vec:vec xy) (boolean rel))
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (let ((w (if rel (add-vert! wer (vec:add (get-vert wer v) xy))
                       (add-vert! wer xy))))
        (declare (pos-int w))
        (add-edge! wer v w :g g)
        w))))


(declaim (inline add-edge?))
(defun add-edge? (v w &key g)
  "create edge between valid verts v and w (in grp g)"
  (declare (pos-int v w))
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (-valid-vert ((weir-num-verts wer) w :err nil)
        (ladd-edge! wer (list v w) :g g)))))

(declaim (inline ladd-edge?))
(defun ladd-edge? (ll &key g)
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (add-edge? a b :g g)))


(declaim (inline del-edge?))
(defun del-edge? (v w &key g)
  "del edge (v w) (of grp g)"
  (declare (pos-int v w))
  (lambda (wer) (del-edge! wer v w :g g)))

(declaim (inline ldel-edge?))
(defun ldel-edge? (ll &key g)
  (declare (list ll))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (del-edge? a b :g g)))


(declaim (inline split-edge-ind?))
(defun split-edge-ind? (v w &key via g)
  "del edge (v w), add edges ((v via) (w via)) (of grp g)"
  (declare (pos-int v w via))
  (lambda (wer) (split-edge-ind! wer v w :via via :g g)))

(declaim (inline lsplit-edge-ind?))
(defun lsplit-edge-ind? (ll &key via g)
  (declare (list ll) (pos-int via))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (split-edge-ind? a b :via via :g g)))


(declaim (inline split-edge?))
(defun split-edge? (v w &key xy g)
  "
  insert a vert, v, on edge e = (v w) such that we get edges (a v) and (v b).
  v will be positioned at xy. returns the new edges (or nil).
  "
  (declare (pos-int v w))
  (lambda (wer)
    (when (edge-exists wer (list v w) :g g)
          (split-edge! wer v w :xy xy :g g))))

(declaim (inline lsplit-edge?))
(defun lsplit-edge? (ll &key xy g)
  (declare (list ll))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (split-edge? a b :xy xy :g g)))

