
(in-package :weir)


(defmacro with-verts-in-rad ((wer xy rad v) &body body)
  (declare (symbol wer))
  (alexandria:with-gensyms (wname)
    `(let ((,wname ,wer))
      (zonemap:with-verts-in-rad ((weir-zonemap ,wname)
                                  (weir-verts ,wname) ,xy ,rad ,v)
        (progn ,@body)))))


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

