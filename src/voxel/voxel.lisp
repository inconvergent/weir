
(in-package :voxels)

(declaim (inline -set-imap))
(defun -set-imap (imap ix iy iz)
  (declare #.*opt-settings* (pos-vec imap) (pos-int ix iy iz))
  (loop for i of-type pos-int from 0 below 24 by 3
        do (setf (aref imap i) (+ ix (aref *offsets* i))
                 (aref imap (1+ i)) (+ iy (aref *offsets* (1+ i)))
                 (aref imap (+ i 2)) (+ iz (aref *offsets* (+ i 2))))))

(declaim (inline -get-cubeindex))
(defun -get-cubeindex (a imap low high)
  (declare #.*opt-settings* (pos-arr a) (pos-vec imap) (pos-int low high))
  (loop with ind of-type pos-int = 0
        for i of-type pos-int from 0 below 24 by 3
        for b of-type pos-int from 0
        do (when (>= high (aref a (aref imap i) (aref imap (1+ i))
                                  (aref imap (+ i 2)))
                    low)
                 (incf ind (the pos-int (expt (the pos-int 2) (the pos-int b)))))
        finally (return ind)))

(declaim (inline -set-voxel-list))
(defun -set-voxel-list (voxellist ec)
  (declare #.*opt-settings* (pos-vec voxellist) (pos-int ec))
  (loop for i of-type pos-int from 0 below 12
        for k of-type pos-int from 0 by 2
        do (let ((pow (the pos-int (expt (the pos-int 2) (the pos-int i)))))
             (declare (pos-int pow))
             (when (= (logand ec pow) pow)
                   (setf (aref voxellist k) (aref *cubeind* k)
                         (aref voxellist (1+ k)) (aref *cubeind* (1+ k)))))))

(declaim (inline -do-global-edge))
(defun -do-global-edge (imap v &aux (3v (* 3 v)))
  (declare #.*opt-settings* (pos-vec imap) (pos-int v 3v))
  (list (aref imap 3v) (aref imap (1+ 3v)) (aref imap (+ 3v 2))))

(declaim (inline -single-ind))
(defun -single-ind (v)
  (declare #.*opt-settings* (list v))
  (destructuring-bind (x y z) v
    (declare (pos-int x y z))
    (+ x (the pos-int
              (* *max-voxels*
                 (the pos-int (+ y (the pos-int (* *max-voxels* z)))))))))

(declaim (inline -hash-edge))
(defun -hash-edge (edge)
  (declare #.*opt-settings* (list edge))
  (sort (mapcar #'-single-ind edge) #'<))


(declaim (inline -intersect))
(defun -intersect (p1 p2 v1 v2)
  (declare #.*opt-settings* (vec:3vec p1 p2) (double-float v1 v2))
  (vec:3smult (vec:3add! (vec:3smult p1 v1) (vec:3smult p2 v2)) (/ (+ v1 v2))))

(declaim (inline -vert->pos))
(defun -vert->pos (ix iy iz)
  (declare #.*opt-settings* (pos-int ix iy iz))
  (vec:3vec (coerce ix 'double-float) (coerce iy 'double-float)
            (coerce iz 'double-float)))

(declaim (inline -get-pos-weighted))
(defun -get-pos-weighted (a e)
  (declare #.*opt-settings* (pos-arr a) (list e))
  (destructuring-bind (v1 v2) e
    (declare (list v1 v2))
    (vec:3add! (-intersect (apply #'-vert->pos v1) (apply #'-vert->pos v2)
                           (coerce (apply #'aref a v1) 'double-float)
                           (coerce (apply #'aref a v2) 'double-float))
               *shift*)))

(declaim (inline -get-pos-mid))
(defun -get-pos-mid (a e)
  (declare #.*opt-settings* (ignore a) (list e))
  (destructuring-bind (v1 v2) e
    (declare (list v1 v2))
    (vec:3add! (vec:3mid (apply #'-vert->pos v1) (apply #'-vert->pos v2)) *shift*)))


(declaim (inline -add-poly))
(defun -add-poly (a edge->vert msh tri posfx)
  (declare #.*opt-settings* (pos-arr a) (hash-table edge->vert) (list tri))
  (mesh:add-polygon! msh
    (loop for e of-type list in tri
          collect (let ((h (-hash-edge e)))
                    (declare (list h))
                    (multiple-value-bind (v exists) (gethash h edge->vert)
                      (declare (boolean exists))
                      (if exists v (setf (gethash h edge->vert)
                                         (mesh:add-vert! msh
                                           (funcall (the function posfx) a e))))))
                  of-type pos-int)))

(declaim (inline -make-poly))
(defun -make-poly (imap voxellist cubeindex i)
  (declare #.*opt-settings* (pos-vec imap voxellist) (pos-int cubeindex i))
  (loop for k of-type pos-int from 0 below 3
        collect (let ((i2 (* 2 (aref *triangles* cubeindex (+ i k)))))
                  (declare (pos-int i2))
                  (list (-do-global-edge imap (aref voxellist i2))
                        (-do-global-edge imap (aref voxellist (1+ i2)))))
                of-type list))

(declaim (inline -add-polys))
(defun -add-polys (a edge->vert imap voxellist cubeindex msh posfx)
  (declare #.*opt-settings* (pos-arr a) (pos-vec imap voxellist)
                            (pos-int cubeindex) (hash-table edge->vert))
  (loop for i of-type pos-int from 0 by 3
        until (= (aref *triangles* cubeindex i) 99)
        do (-add-poly a edge->vert msh
             (-make-poly imap voxellist cubeindex i)
             posfx)))


(defun get-mesh (voxs &key (low 1) (high 1) (weighted nil) (max-verts 10000000))
  (declare #.*opt-settings* (voxels voxs) (pos-int low high max-verts))
  "
  reconstruct mesh at the isosurfaces of [low high] (inclusive).
  "
  (let ((imap (-make-pos-vec 24))
        (voxellist (-make-pos-vec 24))
        (a (voxels-a voxs))
        (edge->vert (make-hash-table :test #'equal :size 1024 :rehash-size 2f0))
        (msh (mesh:make :max-verts max-verts))
        (posfx (if weighted #'-get-pos-weighted #'-get-pos-mid)))
    (declare (pos-vec imap voxellist) (pos-arr a) (hash-table edge->vert)
             (function posfx))
    (loop for ix of-type pos-int from 0 to (voxels-nx voxs)
          do (loop for iy of-type pos-int from 0 to (voxels-ny voxs)
                   do (loop for iz of-type pos-int from 0 to (voxels-nz voxs)
                            do (-set-imap imap ix iy iz)
                               (let* ((cubeindex (-get-cubeindex a imap low high))
                                      (ec (aref *edges* cubeindex)))
                                 (declare (pos-int ec cubeindex))
                                 (unless (or (= ec 0) (= ec 255))
                                   (-set-voxel-list voxellist ec)
                                   (-add-polys a edge->vert imap voxellist
                                               cubeindex msh posfx))))))
    msh))

