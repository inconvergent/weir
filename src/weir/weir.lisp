
(in-package :weir)


(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))


(defstruct (weir (:constructor -make-weir))
  (name :main :type symbol :read-only t)
  (wc 0 :type pos-int)
  (verts nil :type simple-array)
  (num-verts 0 :type pos-int)
  (zonemap nil)
  (kd nil)
  (dim 2 :type pos-int :read-only t)
  (grps (make-hash-table :test #'equal))
  (max-verts 5000 :type pos-int :read-only t)
  (set-size 5 :type pos-int :read-only t)
  (adj-size 100 :type pos-int :read-only t))


(defstruct (grp (:constructor -make-grp))
  (name nil :type symbol :read-only t)
  (grph nil :type graph::graph)
  (type nil :type symbol :read-only t)
  (props nil))


(declaim (inline -exec-alt))
(defun -exec-alt (wer x)
  (declare #.*opt-settings* (weir wer))
  (typecase x (function (funcall (the function x) wer))))

(defmacro with ((wer accfx &key kd zwidth collect) &body body)
  "
  creates a context for manipulating weir via alterations.

  example:

    (weir:with (wer %)
      (% (weir:add-edge? ...)))

  all (% ...) forms inside the weir context will cause the alteration inside to
  be executed, and collected. if it is nil, nothing happens.
  "
  (declare (symbol wer accfx))
  (alexandria:with-gensyms (wname kdname zw x res alts do-alts)
    `(let* ((,wname ,wer)
            (,zw ,zwidth)
            (,alts (list))
            (,kdname ,kd))
      (declare (list ,alts))

      ; build-zonemap and 3build-kdtree will test if the dimension is correct
      ,(when zwidth `(build-zonemap ,wname ,zw))
      ,(when kd `(3build-kdtree ,wname))

      (incf (weir-wc ,wname))

      (labels ((,do-alts ()
                (loop for ,x in ,alts
                      ,(if collect 'collect 'do) (-exec-alt ,wname ,x)))
               (,accfx (,x) (when ,x (push ,x ,alts))))

        (progn ,@body)

        (let ((,res (,do-alts)))
          (declare (list ,res))
          ,(when zwidth `(setf (weir-zonemap ,wname) nil))
          ,(when kd `(setf (weir-kd ,wname) nil))
          ,res)))))


(defmacro with-verts-in-rad ((wer xy rad v) &body body)
  (declare (symbol wer))
  (alexandria:with-gensyms (wname)
    `(let ((,wname ,wer))
      (zonemap:with-verts-in-rad ((weir-zonemap ,wname)
                                  (weir-verts ,wname) ,xy ,rad ,v)
        (progn ,@body)))))


(defmacro with-grp ((wer g* g) &body body)
  "select grp g from weir instance. g will be available in this context as g*"
  (declare (symbol wer))
  (alexandria:with-gensyms (grps exists gname wname)
    `(let ((,wname ,wer)
           (,gname ,g))
      (let ((,grps (weir-grps ,wname)))
        (multiple-value-bind (,g* ,exists)
          (gethash ,gname ,grps)
            (unless ,exists
                    (error "attempted to access invalid group: ~a" ,gname))
            (progn ,@body))))))


(defmacro with-rnd-edge ((wer i &key g) &body body)
  "
  select an arbitrary edge from a weir instance. the edge will be
  available in the context as i.

  if a grp, g, is supplied it will select an edge from g, otherwise it will use
  the main grp.
  "
  (declare (symbol wer))
  (alexandria:with-gensyms (grp edges grph ln)
    `(with-grp (,wer ,grp ,g)
      (let ((,grph (grp-grph ,grp)))
        (let* ((,edges (to-vector (graph:get-edges ,grph)))
               (,ln (length ,edges)))
          (declare (pos-int ,ln))
          (when (> ,ln 0)
                (let ((,i (aref ,edges (rnd:rndi ,ln))))
                  (declare (list ,i))
                  (progn ,@body))))))))


(defmacro with-rnd-vert ((wer i) &body body)
  "
  select an arbitrary vert from a weir instance. the vert will be available in
  the context as i.
  "
  (declare (symbol wer))
  (alexandria:with-gensyms (num)
    `(let ((,num (weir-num-verts ,wer)))
       (when (> ,num 0)
             (let ((,i (rnd:rndi ,num)))
               (declare (pos-int ,i))
               (progn ,@body))))))


(defmacro itr-verts ((wer i &key collect) &body body)
  "iterates over ALL verts in wer as i"
  (declare (symbol wer) (boolean collect))
  (alexandria:with-gensyms (wname)
    `(let ((,wname ,wer))
      (loop for ,i of-type pos-int from 0 below (weir-num-verts ,wname)
            ,(if collect 'collect 'do)
              (progn ,@body)))))


(defmacro itr-grp-verts ((wer i &key g collect) &body body)
  "
  iterates over all verts in grp g as i.

  NOTE: this will only yield vertices that belong to at least one edge that is
  part of g. if you want all vertices in weir you should use itr-verts instead.
  itr-verts is also faster, since it does not rely on the underlying graph
  structure.

  if g is not provided, the main grp wil be used.
  "
  (declare (symbol wer) (boolean collect))
  (alexandria:with-gensyms (grp wname)
    `(let ((,wname ,wer))
      (with-grp (,wname ,grp ,g)
        (map ',(if collect 'list 'nil)
             (lambda (,i) (declare (pos-int ,i))
               (progn ,@body))
             (graph:get-verts (grp-grph ,grp)))))))


(defmacro itr-edges ((wer i &key g collect) &body body)
  "
  iterates over all edges in grp g as i.
  if g is not provided, the main grp will be used.
  "
  (declare (symbol wer) (boolean collect))
  (alexandria:with-gensyms (grp grph)
    `(with-grp (,wer ,grp ,g)
      (let ((,grph (grp-grph ,grp)))
        (map ',(if collect 'list 'nil)
             (lambda (,i)
               (progn ,@body))
             (graph:get-edges ,grph))))))

; TODO: this is slower for some reason
;(defmacro itr-edges* ((wer i &key g) &body body)
;  "
;  iterates over all edges in grp g as i.
;  if g is not provided, the main grp will be used.
;  "
;  (declare (symbol wer))
;  (alexandria:with-gensyms (grp grph)
;    `(with-grp (,wer ,grp ,g)
;      (let ((,grph (grp-grph ,grp)))
;        (graph:with-graph-edges (,grph ,i)
;          (progn ,@body))))))


(defmacro itr-edge-verts ((wer vv &key g) &body body)
  "
  iterates over all edges (as verts) in grp g as i.
  if g is not provided, the main grp will be used.
  "
  (declare (symbol wer))
  (alexandria:with-gensyms (grp grph e)
    `(with-grp (,wer ,grp ,g)
      (let ((,vv nil)
            (,grph (grp-grph ,grp)))
        (graph:with-graph-edges (,grph ,e)
          (setf ,vv (get-verts ,wer ,e))
          (progn ,@body))))))


(defmacro itr-edge-verts* ((wer ee vv &key g) &body body)
  "
  iterates over all edges (as verts) in grp g as i.
  if g is not provided, the main grp will be used.
  "
  (declare (symbol wer))
  (alexandria:with-gensyms (grp grph)
    `(with-grp (,wer ,grp ,g)
      (let ((,vv nil)
            (,grph (grp-grph ,grp)))
        (graph:with-graph-edges (,grph ,ee)
          (setf ,vv (get-verts ,wer ,ee))
          (progn ,@body))))))


(defmacro 3itr-edge-verts ((wer vv &key g) &body body)
  "
  iterates over all edges (as verts) in grp g as i.
  if g is not provided, the main grp will be used.
  "
  (declare (symbol wer))
  (alexandria:with-gensyms (grp grph e)
    `(with-grp (,wer ,grp ,g)
      (let ((,vv nil)
            (,grph (grp-grph ,grp)))
        (graph:with-graph-edges (,grph ,e)
          (setf ,vv (3get-verts ,wer ,e))
          (progn ,@body))))))


(defmacro 3itr-edge-verts* ((wer ee vv &key g) &body body)
  "
  iterates over all edges (as verts) in grp g as i.
  if g is not provided, the main grp will be used.
  "
  (declare (symbol wer))
  (alexandria:with-gensyms (grp grph)
    `(with-grp (,wer ,grp ,g)
      (let ((,vv nil)
            (,grph (grp-grph ,grp)))
        (graph:with-graph-edges (,grph ,ee)
          (setf ,vv (3get-verts ,wer ,ee))
          (progn ,@body))))))


(defmacro itr-grps ((wer g &key collect main) &body body)
  "iterates over all grps of wer as g"
  (declare (symbol wer) (boolean collect))
  (alexandria:with-gensyms (grps wname main*)
    `(let ((,wname ,wer)
           (,main* ,main))
      (let ((,grps (weir-grps ,wname)))
        (loop for ,g being the hash-keys of ,grps
          if (or ,main* ,g)
          ,(if collect 'collect 'do)
          (progn ,@body))))))


(defun -make-hash-table (&key init (test #'equal))
  (declare (list init) (function test))
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


(defun add-grp! (wer &key type name props &aux (name* (if name name (gensym))))
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
  (declare (weir wer))
  (with-struct (weir- grps adj-size set-size) wer
    (multiple-value-bind (v exists) (gethash name* grps)
      (declare (ignore v) (boolean exists))
      (when exists (error "grp name already exists: ~a" name*)))
    (setf (gethash name* grps) (-make-grp
                                 :name name*
                                 :type type
                                 :grph (graph:make :adj-size adj-size
                                                   :set-size set-size)
                                 :props props)))
  name*)


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


(defun get-grp-props (wer &key g)
  (declare (weir wer))
  (grp-props (get-grp wer :g g)))


(defun set-grp-props! (wer v &key g)
  (declare (weir wer))
  (setf (grp-props (get-grp wer :g g)) v))


(defun get-all-grps (wer &key main)
  "returns all grps. use :main t to include main/nil grp"
  (declare (weir wer) (boolean main))
  (loop for g being the hash-keys of (weir-grps wer)
        ; ignores nil (main) grp, unless overridden with :main t
        if (or g main) collect g))


(defun get-grp (wer &key g)
  "returns the grp g. if g is not provided, the main/nil grp will be returned"
  (declare (weir wer))
  (gethash g (weir-grps wer)))


(defun get-num-edges (wer &key g)
  (declare (weir wer))
  (with-grp (wer g* g)
    (graph:get-num-edges (grp-grph g*))))


(defun get-num-grps (wer)
  (declare (weir wer))
  (1- (hash-table-count (weir-grps wer))))


(defun get-edges (wer &key g)
  (declare (weir wer))
  (with-grp (wer g* g)
    (graph:get-edges (grp-grph g*))))


; TODO: get-all-incident-edges (not just in grp g)?
(defun get-incident-edges (wer v &key g)
  (declare (weir wer) (pos-int v))
  (with-grp (wer g* g)
    (graph:get-incident-edges (grp-grph g*) v)))


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
  (declare (weir wer) (list ee))
  (with-grp (wer g* g)
    (apply #'graph:mem (grp-grph g*) ee)))


(defun add-edge! (wer a b &key g)
  "
  adds a new edge to weir. provided the edge is valid.
  otherwise it returns nil.

  returns nil if the edge exists already.
  "
  (declare (weir wer) (pos-int a b))
  (when (= a b) (return-from add-edge! nil))
  (with-grp (wer g* g)
    (with-struct (weir- num-verts) wer
      (declare (pos-int num-verts))
      (with-struct (grp- grph) g*
        (when (and (< a num-verts) (< b num-verts))
              (when (graph:add grph a b)
                    (sort (list a b) #'<)))))))


(defun ladd-edge! (wer ee &key g)
  (declare (weir wer) (list ee))
  (destructuring-bind (a b) ee
    (declare (pos-int a b))
    (add-edge! wer a b :g g)))


(defun add-edges! (wer ee &key g)
  "adds multiple edges (see above). returns a list of the results"
  (declare (weir wer) (list ee))
  (loop for e of-type list in ee collect (ladd-edge! wer e :g g)))


(defun del-edge! (wer a b &key g)
  (declare (weir wer) (pos-int a b))
  (with-grp (wer g* g)
    (with-struct (grp- grph) g*
      (graph:del grph a b))))


(defun ldel-edge! (wer ee &key g)
  (declare (weir wer) (list ee))
  (with-grp (wer g* g)
    (with-struct (grp- grph) g*
      (apply #'graph:del grph ee))))


(defun split-edge-ind! (wer a b &key via g)
  (declare (weir wer) (pos-int a b via))
  (when (del-edge! wer a b :g g)
        (list (add-edge! wer a via :g g)
              (add-edge! wer via b :g g))))

(defun lsplit-edge-ind! (wer ee &key via g)
  (declare (weir wer) (list ee) (pos-int via))
  (destructuring-bind (a b) ee
    (declare (pos-int a b))
    (split-edge-ind! wer a b :via via :g g)))


(declaim (inline -roll-once))
(defun -roll-once (aa)
  (declare (list aa))
  (butlast (append (last aa) aa) 1))

(defun add-path-ind! (wer vv &key g closed)
  (loop for a of-type pos-int in vv
        and b of-type pos-int in
          (funcall (the function (if closed #'-roll-once #'cdr)) vv)
        collect (add-edge! wer a b :g g)))

