(in-package :weir)

(declaim (inline -dimtest))
(defun -dimtest (wer)
  (declare #.*opt-settings* (weir wer))
  (when (not (= (weir-dim wer) 2)) (error "wrong dimension, use 2.")))

(defun get-verts (wer vv &aux (vv* (if (equal (type-of vv) 'cons)
                                       vv (to-list vv))))
  "
  get the coordinates (vec) of verts in vv
  "
  (declare #.*opt-settings* (weir wer) (sequence vv))
  (-dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (-valid-verts (num-verts vv* v)
      (avec:getv verts v))))


(defun get-grp-verts (wer &key g order)
  "
  returns all vertices in grp g.
  note: verts only belong to a grp if they are part of an edge in grp.
  "
  (declare #.*opt-settings* (weir wer) (boolean order))
  (get-verts wer (get-vert-inds wer :g g :order order)))


(defun is-vert-in-grp (wer v &key g)
  "
  tests whether v is in grp g
  note: verts only belong to a grp if they are part of an edge in grp.
  "
  (declare #.*opt-settings* (weir wer) (pos-int v))
  (with-struct (weir- grps) wer
    (multiple-value-bind (g* exists) (gethash g grps)
      (if exists (graph:vmem (grp-grph g*) v)
                 (error "grp does not exist: ~a" g)))))


(defun get-num-verts (wer)
  (declare #.*opt-settings* (weir wer))
  (weir-num-verts wer))


(defun get-grp-num-verts (wer &key g)
  (declare #.*opt-settings* (weir wer))
  (with-grp (wer g* g)
    (graph:get-num-verts (grp-grph g*))))


(defun add-vert! (wer xy)
  "
  adds a new vertex to weir. returns the new vert ind.
  "
  (declare #.*opt-settings* (weir wer) (vec:vec xy))
  (-dimtest wer)
  (with-struct (weir- verts num-verts max-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts max-verts))
    (when (>= num-verts max-verts)
      (error "too many verts ~a~%." num-verts))
    (avec:setv verts num-verts xy))
  (1- (incf (weir-num-verts wer))))


(defun add-verts! (wer vv)
  "
  adds new vertices to weir. returns the ids of the new vertices
  "
  (declare #.*opt-settings* (weir wer) (list vv))
  (-dimtest wer)
  (loop for xy of-type vec:vec in vv
        collect (add-vert! wer xy)))


(defun get-vert (wer v)
  "
  get the coordinate (vec) of vert v.
  "
  (declare #.*opt-settings* (weir wer) (pos-int v))
  (-dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (-valid-vert (num-verts v)
      (avec:getv verts v))))


(defun get-all-verts (wer)
  "
  returns the coordinates (vec) of all vertices.
  "
  (declare #.*opt-settings* (weir wer))
  (-dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (loop for v of-type pos-int from 0 below num-verts
          collect (avec:getv verts v) of-type vec:vec)))


(defun make-vert-getter (wer)
  (declare (weir wer))
  (let ((verts (to-vector (weir:get-all-verts wer))))
    (declare (type simple-array verts))
    (lambda (vv) (declare (list vv))
      (mapcar (lambda (i) (declare (pos-int i))
                (aref verts i))
              vv))))


(declaim (inline move-vert!))
(defun move-vert! (wer i v &key (rel t) (ret nil))
  (declare #.*opt-settings*
           (weir wer) (pos-int i) (vec:vec v) (boolean rel))
  (-dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (when (>= i num-verts)
          (error "attempting to move invalid vert, ~a (~a)" i num-verts))
    (vec:with-xy (v vx vy)
      (avec:with-vec (verts i x y)
        (if rel (setf x (+ x vx) y (+ y vy))
                (setf x vx y vy))
        (when ret (vec:vec x y))))))


(defun transform! (wer inds fx)
  (declare (weir wer) (function fx) (list inds))
  (-dimtest wer)
  (let* ((verts (get-verts wer inds)))
    (declare (list verts))
    (loop for i of-type pos-int in inds
          and p of-type vec:vec in (funcall fx verts)
          do (move-vert! wer i p :rel nil))))

(defun grp-transform! (wer fx &key g)
  (declare (weir wer) (function fx))
  (transform! wer (get-vert-inds wer :g g) fx))


(defun split-edge! (wer u v &key xy g)
  "
  split edge at xy (or middle if xy is nil).
  returns new vert ind (and new edges).
  "
  (declare #.*opt-settings* (weir wer) (pos-int u v) (vec:vec xy))
  (-dimtest wer)
  (if (del-edge! wer u v :g g)
      (let ((c (add-vert! wer xy)))
        (declare (pos-int c))
        (let ((edges (list (add-edge! wer c u :g g)
                           (add-edge! wer c v :g g))))
          (declare (list edges))
          (values c edges)))
      (values nil nil)))

(defun lsplit-edge! (wer ll &key xy g)
  (declare #.*opt-settings* (weir wer) (list ll) (vec:vec xy))
  (-dimtest wer)
  (destructuring-bind (a b) ll
    (declare (pos-int a b))
    (split-edge! wer a b :xy xy :g g)))


(defun verts-in-rad (wer xy rad)
  (declare #.*opt-settings* (weir wer) (vec:vec xy) (double-float rad))
  (-dimtest wer)
  (with-struct (weir- verts zonemap) wer
    (declare (type (simple-array double-float) verts))
    (zonemap:verts-in-rad zonemap verts xy rad)))


(defun edge-length (wer a b)
  "
  returns the length of edge (a b).
  "
  (declare #.*opt-settings* (weir wer) (pos-int a b))
  (-dimtest wer)
  (with-struct (weir- verts) wer
    (declare (type (simple-array double-float) verts))
    (avec:dst verts verts a b)))


(defun ledge-length (wer e)
  "
  returns the length of edge e=(a b).
  "
  (declare #.*opt-settings* (weir wer) (list e))
  (-dimtest wer)
  (apply #'edge-length wer e))


(defun prune-edges-by-len! (wer lim &optional (fx #'>))
  "
  remove edges longer than lim, use fx #'< to remove edges shorter than lim.
  "
  (declare #.*opt-settings* (weir wer) (double-float lim) (function fx))
  (-dimtest wer)
  (itr-edges (wer e)
    (when (funcall (the function fx) (ledge-length wer e) lim)
          (ldel-edge! wer e))))


(declaim (inline -center))
(defun -center (verts v mid mx my &key (s 1d0))
  (declare #.*opt-settings* (type (simple-array double-float) verts)
           (pos-int v) (vec:vec mid) (double-float mx my s))
  (avec:with-vec (verts v x y)
    (setf x (+ (vec:vec-x mid) (* s (- x mx)))
          y (+ (vec:vec-y mid) (* s (- y my))))))

(declaim (inline -scale-by))
(defun -scale-by (max-side sx sy)
  (declare #.*opt-settings* (double-float sx sy))
  (cond ((not max-side) 1d0)
        ((> sx sy) (/ (the double-float max-side) sx))
        (t (/ (the double-float max-side) sy))))

(defun center! (wer &key (xy vec:*zero*) max-side (non-edge t) g)
  "
  center the verts of wer on xy. returns the previous center.
  "
  (-dimtest wer)
  (with-struct (weir- verts num-verts) wer
    (declare (type (simple-array double-float) verts)
             (pos-int num-verts))
    (multiple-value-bind (minx maxx miny maxy)
      (if non-edge (avec:minmax verts num-verts)
                   (avec::minmax* verts (get-vert-inds wer :g g)))
      (let* ((mx (* 0.5d0 (+ minx maxx)))
             (my (* 0.5d0 (+ miny maxy)))
             (w (- maxx minx))
             (h (- maxy miny))
             (s (-scale-by max-side w h)))
        (declare (double-float mx my))
        (itr-verts (wer v) (-center verts v xy mx my :s s))
        (values (vec:vec mx my) (vec:vec w h) s)))))

(defun cut-to-area! (wer w h &key g)
  "
  removes all edges (in g) outside envelope (0d0 0d0), (w h).
  all edges intersecting the envelope will be deleted, a new vert will be
  inserted on the intersection. connected to the inside vert.
  edges inside the envelope will be left as they are.
  "
  (declare (weir wer) (double-float w h))
  (labels
    ((inside (pt)
      (declare (vec:vec pt))
      (vec:with-xy (pt x y) (and (>= x 0d0) (>= y 0d0) (<= x w) (<= y h))))

     (split-line (line &aux (rev nil))
       (declare (list line) (boolean rev))
       (unless (inside (first line)) (setf line (reverse line) rev t))
       (destructuring-bind (a b) line
         (declare (vec:vec a b))
         (return-from split-line
           (vec:with-xy (a xa ya)
             (vec:with-xy (b xb yb)
               (list rev (vec:lon-line
                           (cond ((> xb w) (/ (- w xa) (- xb xa)))
                                 ((> yb h) (/ (- h ya) (- yb ya)))
                                 ((< xb 0d0) (/ (- xa) (- xb xa)))
                                 ((< yb 0d0) (/ (- ya) (- yb ya))))
                           line)))))))

     (cutfx (line)
       (declare (list line))
       (let ((c (length (remove-if-not (lambda (v) (inside v)) line))))
         (declare (fixnum c))
         (cond ((= c 0) (values :none nil nil))
               ((= c 1) (destructuring-bind (rev pt) (split-line line)
                          (values :split rev pt)))
               (t (values :keep nil nil))))))

    (with (wer %)
      (itr-edges (wer e :g g)
        (multiple-value-bind (state rev pt) (cutfx (get-verts wer e))
          (declare (symbol state) (boolean rev) (vec:vec rev))
          (cond ((equal state :keep) t)
                ((equal state :none) (% (ldel-edge? e :g g)))
                ((equal state :split)
                   (% (ldel-edge? e :g g))
                   (% (append-edge?
                        (if rev (second e) (first e)) pt :rel nil :g g)))))))))


(defun build-zonemap (wer rad)
  (declare (weir wer) (double-float rad))
  (-dimtest wer)
  (setf (weir-zonemap wer)
        (zonemap:make (weir-verts wer) (weir-num-verts wer) rad)))


(declaim (inline -is-rel-neigh))
(defun -is-rel-neigh (verts u v near)
  (declare #.*opt-settings* (type (simple-array double-float) verts)
                            (pos-int u v) (list near))
  (loop with d of-type double-float = (avec:dst2 verts verts u v)
        for w of-type pos-int in near
        if (not (> (the double-float
                        (max (the double-float (avec:dst2 verts verts u w))
                             (the double-float (avec:dst2 verts verts v w)))) d))
          summing 1 into c of-type pos-int
        ; TODO: avoid this by stripping u from near
        if (> c 1) do (return-from -is-rel-neigh nil))
  t)

; TODO: this is stil more than a little inefficient
(defun relative-neighborhood! (wer rad &key g)
  "
  find the relative neigborhood graph (limited by the radius rad) of verts
  in wer. the graph is made in grp g.
  "
  (declare #.*opt-settings* (weir wer) (double-float rad))
  (-dimtest wer)
  (build-zonemap wer rad)
  (let ((c 0))
    (declare (pos-int c))
    (itr-verts (wer v)
      (loop with verts of-type (simple-array double-float) = (weir-verts wer)
            with near of-type list =
              (to-list (remove-if (lambda (x)
                                    (declare (pos-int x))
                                    (= x v))
                                  (verts-in-rad wer (get-vert wer v) rad)))
            ; TODO: strip u from near
            for u of-type pos-int in near
            if (and (< u v) (-is-rel-neigh verts u v near))
            do (when (add-edge! wer u v :g g) (incf c))))
    c))


(defun add-path! (wer points &key g closed)
  (declare #.*opt-settings* (weir wer) (list points))
  (-dimtest wer)
  (add-path-ind! wer (add-verts! wer points) :g g :closed closed))


(defun export-verts-grp (wer &key g)
  "
  export verts, as well as the edges in g, on the format (verts edges)
  "
  (declare #.*opt-settings* (weir wer))
  (-dimtest wer)
  (list (get-all-verts wer) (get-edges wer :g g)))


(defun import-verts-grp (wer o &key g)
  "
  import data exported using export-verts-grp.
  "
  (declare #.*opt-settings* (weir wer) (list o))
  (-dimtest wer)
  (when (> (get-num-verts wer) 0)
        (error "ensure there are no initial verts in wer."))
  (destructuring-bind (verts edges) o
    (declare (list verts edges))
    (add-verts! wer verts)
    (loop for e of-type list in edges do (ladd-edge! wer e :g g))))

