
(in-package :weir)


(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))


(defstruct (weir (:constructor -make-weir))
  (name :main :type symbol :read-only t)
  (adj-size 100 :type pos-int :read-only t)
  (alt-res (make-hash-table :test #'equal :size 20 :rehash-size 2f0))
  (dim 2 :type pos-int :read-only t)
  (grps (make-hash-table :test #'equal :size 20 :rehash-size 2f0))
  (props (make-hash-table :test #'equal :size 500 :rehash-size 2f0))
  (max-verts 5000 :type pos-int :read-only t)
  (num-verts 0 :type pos-int)
  (set-size 5 :type pos-int :read-only t)
  (verts nil :type simple-array)
  (wc 0 :type pos-int)
  (kd nil)
  (zonemap nil))


(defstruct (grp (:constructor -make-grp))
  (name nil :type symbol :read-only t)
  (grph nil :type graph::graph)
  (type nil :type symbol :read-only t))


(declaim (inline -exec-alt))
(defun -exec-alt (wer x)
  (declare #.*opt-settings* (weir wer))
  (typecase x (function (funcall (the function x) wer))))


(defun exec-alt (wer x)
  (declare #.*opt-settings* (weir wer))
  (-exec-alt wer x))

(defun lexec-alt (wer alts &key collect)
  (declare #.*opt-settings* (weir wer))
  (if collect (loop for x in alts collect (-exec-alt wer x))
              (loop for x in alts do (-exec-alt wer x))))


(defun -make-hash-table (&key init (test #'equal))
  (declare #.*opt-settings* (list init) (function test))
  (loop with res = (make-hash-table :test test)
        for (k v) in init
        do (setf (gethash k res) v)
        finally (return res)))

(defun make (&key (max-verts 5000) (adj-size 4)
                  (set-size 10) name (dim 2))
  "
  make weir instances

  - max-verts is the maximum number of verts in weir (across all grps).
  - set-size is the initial size of edge adjacency sets.
      ie. the expected number of vertices in the graph
  - adj-size is the initial size of the adjacency map.
      ie. the expected number of incident vertices
  - dim is the vector dimension of vertices
  "
  (declare (pos-int max-verts adj-size set-size dim) (symbol name))
  (when (not (> 4 dim 1)) (error "dim must be 2 or 3."))
  (-make-weir
    :name name
    :verts (avec:avec max-verts :dim dim)
    :max-verts max-verts
    :set-size set-size
    :adj-size adj-size
    :dim dim
    :grps (-make-hash-table
            :init (list (list nil (-make-grp
                                    :name :main :type :main
                                    :grph (graph:make :adj-size adj-size
                                                      :set-size set-size)))))))


(defun add-grp! (wer &key type name
                     &aux (name* (if name name (gensym "grp"))))
  (declare #.*opt-settings* (weir wer))
  "
  constructor for grp instances.

  grps are edge graphs.

  nil is the default grp. as such, nil is not an allowed grp name (there is
  always a default grp named nil). if name is nil, the name will be a gensym.

  edges can be associated with multiple grps.

  verts are global. that is, they do not belong to any grp on their own.
  however, if a vert is associated with an edge, that vert is also associated
  with whatever grp that edge belongs to.

    - to get verts in a grp: (get-grp-verts wer :g g).
    - to get indices of verts (in a grp): (get-vert-inds wer :g g)
    - ...

  the grp functionality is somewhat experimental.
  "
  (with-struct (weir- grps adj-size set-size) wer
    (multiple-value-bind (v exists) (gethash name* grps)
      (declare (ignore v) (boolean exists))
      (when exists (error "grp name already exists: ~a" name*)))
    (setf (gethash name* grps) (-make-grp
                                 :name name*
                                 :type type
                                 :grph (graph:make :adj-size adj-size
                                                   :set-size set-size))))
  name*)

(defun del-grp! (wer &key g)
  (declare #.*opt-settings* (symbol g))
  (remhash g (weir-grps wer)))


(defmacro with-grp ((wer g* g) &body body)
  (declare (symbol wer))
  "select grp g from weir instance. g will be available in this context as g*"
  (alexandria:with-gensyms (grps exists gname wname)
    `(let ((,wname ,wer)
           (,gname ,g))
      (let ((,grps (weir-grps ,wname)))
        (multiple-value-bind (,g* ,exists)
          (gethash ,gname ,grps)
            (unless ,exists
                    (error "attempted to access invalid group: ~a" ,gname))
            (progn ,@body))))))


(defmacro -valid-vert ((num v &key (err t)) &body body)
  (alexandria:with-gensyms (v* num*)
    `(let ((,v* ,v)
           (,num* ,num))
      (declare (pos-int ,v* ,num*))
      (if (< -1 ,v* ,num*)
        (progn ,@body)
        ,(when err `(error "vert does not exist: ~a" ,v*))))))


(defmacro -valid-verts ((num vv v) &body body)
  (alexandria:with-gensyms (vv* num*)
    `(let ((,vv* ,vv)
           (,num* ,num))
      (declare (list ,vv*) (pos-int ,num*))
      (loop for ,v of-type pos-int in ,vv*
            if (< -1 ,v ,num*) collect (progn ,@body)))))


(defun get-all-grps (wer &key main)
  (declare #.*opt-settings* (weir wer) (boolean main))
  "returns all grps. use :main t to include main/nil grp"
  (loop for g being the hash-keys of (weir-grps wer)
        ; ignores nil (main) grp, unless overridden with :main t
        if (or g main) collect g))


(defun get-grp (wer &key g)
  (declare #.*opt-settings* (weir wer))
  "returns the grp g. if g is not provided, the main/nil grp will be returned"
  (gethash g (weir-grps wer)))


(defun get-num-edges (wer &key g)
  (declare #.*opt-settings* (weir wer))
  (with-grp (wer g* g)
    (graph:get-num-edges (grp-grph g*))))


(defun get-num-grps (wer)
  (declare #.*opt-settings* (weir wer))
  (1- (hash-table-count (weir-grps wer))))


(defun get-edges (wer &key g)
  (declare #.*opt-settings* (weir wer))
  (with-grp (wer g* g)
    (graph:get-edges (grp-grph g*))))

(defun get-grp-as-path (wer &key g)
  (declare #.*opt-settings* (weir wer))
  "returns (values path cycle?)"
  (graph:edge-set->path (weir:get-edges wer :g g)))


; TODO: get-all-incident-edges (not just in grp g)?
(defun get-incident-edges (wer v &key g)
  (declare #.*opt-settings* (weir wer) (pos-int v))
  (with-grp (wer g* g)
    (graph:get-incident-edges (grp-grph g*) v)))

(defun get-incident-verts (wer v &key g)
  (declare #.*opt-settings* (weir wer) (pos-int v))
  (with-grp (wer g* g)
    (graph:get-incident-verts (grp-grph g*) v)))


(defun get-vert-inds (wer &key g order)
  "
  returns all vertex indices that belongs to a grp.
  note: verts only belong to a grp if they are part of an edge in grp.
  "
  (declare #.*opt-settings* (weir wer) (boolean order))
  (with-struct (weir- grps) wer
    (multiple-value-bind (g* exists) (gethash g grps)
      (if exists (if order (sort (the list (graph:get-verts (grp-grph g*))) #'<)
                           (graph:get-verts (grp-grph g*)))
                 (error "grp does not exist: ~a" g)))))


(defun edge-exists (wer ee &key g)
  (declare #.*opt-settings* (weir wer) (list ee))
  (with-grp (wer g* g)
    (apply #'graph:mem (grp-grph g*) ee)))


(defun add-edge! (wer a b &key g)
  "
  adds a new edge to weir. provided the edge is valid.
  otherwise it returns nil.

  returns nil if the edge exists already.
  "
  (declare #.*opt-settings* (weir wer) (pos-int a b))
  (when (= a b) (return-from add-edge! nil))
  (with-grp (wer g* g)
    (with-struct (weir- num-verts) wer
      (declare (pos-int num-verts))
      (with-struct (grp- grph) g*
        (when (and (< a num-verts) (< b num-verts))
              (when (graph:add grph a b)
                    (sort (list a b) #'<)))))))


(defun ladd-edge! (wer ee &key g)
  (declare #.*opt-settings* (weir wer) (list ee))
  (destructuring-bind (a b) ee
    (declare (pos-int a b))
    (add-edge! wer a b :g g)))


(defun add-edges! (wer ee &key g)
  "adds multiple edges (see above). returns a list of the results"
  (declare #.*opt-settings* (weir wer) (list ee))
  (loop for e of-type list in ee collect (ladd-edge! wer e :g g)))


(defun del-edge! (wer a b &key g)
  (declare #.*opt-settings* (weir wer) (pos-int a b))
  (with-grp (wer g* g)
    (with-struct (grp- grph) g*
      (graph:del grph a b))))


(defun ldel-edge! (wer ee &key g)
  (declare #.*opt-settings* (weir wer) (list ee))
  (with-grp (wer g* g)
    (with-struct (grp- grph) g*
      (apply #'graph:del grph ee))))


(defun split-edge-ind! (wer a b &key via g)
  (declare #.*opt-settings* (weir wer) (pos-int a b via))
  (when (del-edge! wer a b :g g)
        (list (add-edge! wer a via :g g)
              (add-edge! wer via b :g g))))

(defun lsplit-edge-ind! (wer ee &key via g)
  (declare #.*opt-settings* (weir wer) (list ee) (pos-int via))
  (destructuring-bind (a b) ee
    (declare (pos-int a b))
    (split-edge-ind! wer a b :via via :g g)))


(declaim (inline -roll-once))
(defun -roll-once (aa)
  (declare #.*opt-settings* (list aa))
  (butlast (append (last aa) aa) 1))

(defun add-path-ind! (wer vv &key g closed)
  (loop for a of-type pos-int in vv
        and b of-type pos-int in
          (funcall (the function (if closed #'-roll-once #'cdr)) vv)
        collect (add-edge! wer a b :g g)))

(weir-utils:abbrev gvs get-verts)
(weir-utils:abbrev gv get-vert)
(weir-utils:abbrev 3gvs 3get-verts)
(weir-utils:abbrev 3gv 3get-vert)

