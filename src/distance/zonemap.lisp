
(in-package :zonemap)


(deftype int (&optional (bits 31))
  `(signed-byte ,bits))


(declaim (inline -xy-to-zone))
(defun -xy-to-zone (zwidth x y)
  (declare #.*opt-settings*
           (double-float zwidth x y))
  (values (the int (floor x zwidth))
          (the int (floor y zwidth))))


(defstruct (zonemap (:constructor -make-zonemap))
  (zwidth nil :type double-float :read-only t)
  (num-verts nil :type int :read-only t)
  (zone-to-verts nil :type hash-table :read-only t))


(defun make (verts num-verts zwidth)
  (declare #.*opt-settings*
           (double-float zwidth) (type int num-verts)
           (type (simple-array double-float) verts))
  (let ((zone-to-verts (make-hash-table :size 40 :rehash-size 2f0 :test #'equal)))
    (loop for v of-type int from 0 below num-verts do
      (let ((z (list (floor (aref verts #1=(* 2 v)) zwidth)
                     (floor (aref verts (1+ #1#)) zwidth))))
        (declare (list z))
        (multiple-value-bind (vals exists) (gethash z zone-to-verts)
          (when (not exists)
            (setf vals (make-adjustable-vector :type 'int)
                  (gethash z zone-to-verts) vals))
          (vextend v vals))))

    (-make-zonemap :zwidth zwidth :num-verts num-verts
                :zone-to-verts zone-to-verts)))


(defmacro with-verts-in-rad ((zm verts xy rad v) &body body)
  (alexandria:with-gensyms (rad2 zm* zwidth zone-to-verts xy* za zai zb
                 vals verts* exists i j xx yy)
    `(let* ((,rad2 (expt ,rad 2d0))
            (,verts* ,verts)
            (,xy* ,xy)
            (,xx (vec:vec-x ,xy*))
            (,yy (vec:vec-y ,xy*))
            (,zm* ,zm)
            (,zwidth (zonemap-zwidth ,zm*))
            (,zone-to-verts (zonemap-zone-to-verts ,zm*)))
      (declare (double-float ,rad2 ,zwidth ,xx ,yy)
               (type (simple-array double-float) ,verts*)
               (hash-table ,zone-to-verts) (vec:vec ,xy*))
      (multiple-value-bind (,za ,zb) (-xy-to-zone ,zwidth ,xx ,yy)
        (declare (type int ,za ,zb))
        (loop for ,i of-type int from -1 below 2 do
          (loop with ,zai of-type int = (+ ,za ,i)
                for ,j of-type int from -1 below 2 do
            (multiple-value-bind (,vals ,exists)
              (gethash (list ,zai (+ ,j ,zb)) ,zone-to-verts)
              (when ,exists
                (loop for ,v of-type int across ,vals
                      if (< (+ (expt (- ,xx (aref ,verts*
                                                  (the int (* 2 ,v)))) 2d0)
                               (expt (- ,yy (aref ,verts*
                                                  (the int (1+ (* 2 ,v))))) 2d0))
                            ,rad2)
                      do (progn ,@body))))))))))

(declaim (inline verts-in-rad))
(defun verts-in-rad (zm verts xy rad)
  (declare #.*opt-settings*
           (type (simple-array double-float) verts) (zonemap zm)
           (vec:vec xy) (double-float rad))
  (let ((inds (make-adjustable-vector :type 'int)))
    (declare (vector inds))
    (with-verts-in-rad (zm verts xy rad v) (vextend (the int v) inds))
    inds))

