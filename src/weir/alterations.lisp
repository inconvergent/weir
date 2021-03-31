
(in-package :weir)


(declaim (inline add-grp?))
(defun add-grp? (&key name type)
  (lambda (wer) (add-grp! wer :name name :type type)))


(declaim (inline add-vert?))
(defun add-vert? (xy)
  (declare (vec:vec xy))
  "add new vert at xy. returns the new vert ind"
  (lambda (wer) (add-vert! wer xy)))


(declaim (inline vadd-edge?))
(defun vadd-edge? (xya xyb &key g)
  (declare (vec:vec xya xyb))
  "add verts xya and xyb, and creates an edge (in grp g) between them"
  (lambda (wer) (add-edge! wer (add-vert! wer xya) (add-vert! wer xyb) :g g)))


(declaim (inline move-vert?))
(defun move-vert? (v xy &key (rel t))
  (declare (pos-int v) (vec:vec xy) (boolean rel))
  "
  move vert v.
    if rel: move relative to original position.
    else: move to xy.
  "
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (move-vert! wer v xy :rel rel :ret t))))


(declaim (inline append-edge?))
(defun append-edge? (v xy &key (rel t) g)
  (declare (pos-int v) (vec:vec xy) (boolean rel))
  "add edge between vert v and new vert at xy"
  (lambda (wer)
    (-valid-vert ((weir-num-verts wer) v :err nil)
      (let ((w (if rel (add-vert! wer (vec:add (get-vert wer v) xy))
                       (add-vert! wer xy))))
        (declare (pos-int w))
        (add-edge! wer v w :g g)
        w))))


(declaim (inline add-edge?))
(defun add-edge? (v w &key g)
  (declare (pos-int v w))
  "create edge between valid verts v and w (in grp g)"
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
  (declare (pos-int v w))
  "del edge (v w) (of grp g)"
  (lambda (wer) (del-edge! wer v w :g g)))

(declaim (inline ldel-edge?))
(defun ldel-edge? (ll &key g)
  (declare (list ll))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (del-edge? a b :g g)))


(declaim (inline add-path?))
(defun add-path? (points &key g closed)
  (declare (list points) (boolean closed))
  (lambda (wer) (add-path! wer points :g g :closed closed)))


(declaim (inline split-edge-ind?))
(defun split-edge-ind? (v w &key via g)
  (declare (pos-int v w via))
  "del edge (v w), add edges ((v via) (w via)) (of grp g)"
  (lambda (wer) (split-edge-ind! wer v w :via via :g g)))

(declaim (inline lsplit-edge-ind?))
(defun lsplit-edge-ind? (ll &key via g)
  (declare (list ll) (pos-int via))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (split-edge-ind? a b :via via :g g)))


(declaim (inline split-edge?))
(defun split-edge? (v w &key xy g)
  (declare (pos-int v w))
  "
  insert a vert, v, on edge e = (v w) such that we get edges (a v) and (v b).
  v will be positioned at xy. returns the new edges (or nil).
  "
  (lambda (wer)
    (when (edge-exists wer (list v w) :g g)
          (split-edge! wer v w :xy xy :g g))))

(declaim (inline lsplit-edge?))
(defun lsplit-edge? (ll &key xy g)
  (declare (list ll))
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (split-edge? a b :xy xy :g g)))


(defun set-edge-prop? (edge prop &optional (val t))
  (declare (list edge) (symbol prop))
  (lambda (wer) (setf (get-edge-prop wer edge prop) val)))

(defun lset-edge-prop? (edges prop &optional (val t))
  (declare (list edges) (symbol prop))
  (lambda (wer) (lset-edge-prop wer edges prop val)))


(defun set-vert-prop? (v prop &optional (val t))
  (declare (fixnum v) (symbol prop))
  (lambda (wer) (setf (get-vert-prop wer v prop) val)))

(defun lset-vert-prop? (verts prop &optional (val t))
  (declare (list verts) (symbol prop))
  (lambda (wer) (lset-vert-prop wer verts prop val)))


(defun set-grp-prop? (g prop &optional (val t))
  (declare (symbol g) (symbol prop))
  (lambda (wer) (setf (get-grp-prop wer g prop) val)))

; alterations that return a value, but don't do anything
; postfixed with ...%

(defun get-vert-prop% (v prop)
  (declare (fixnum v) (symbol prop))
  (lambda (wer) (get-vert-prop wer v prop)))

(defun get-edge-prop% (edge prop)
  (declare (list edge) (symbol prop))
  (lambda (wer) (get-edge-prop wer edge prop)))

(defun get-grp-prop% (g prop)
  (declare (symbol g) (symbol prop))
  (lambda (wer) (get-grp-prop wer g prop)))

(defun verts-with-prop% (prop &key val)
  (declare (symbol prop))
  (lambda (wer) (verts-with-prop wer prop :val val)))

(defun edges-with-prop% (prop &key val)
  (declare (symbol prop))
  (lambda (wer) (edges-with-prop wer prop :val val)))

