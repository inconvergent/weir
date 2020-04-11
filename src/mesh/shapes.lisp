
(in-package :mesh)


(defun add-box! (msh &key (sx 1d0) (sy 1d0) (sz 1d0) (xy vec:*3zero*))
  (declare (mesh msh) (double-float sx sy sz) (vec:3vec xy))
  (let* ((verts (make-hash-table :test #'equal))
         (gp (lambda (l) (declare (list l))
               (loop for v of-type list in l collect (gethash v verts))))
         (addvert (lambda (xyzi xyz) (declare (list xyzi) (vec:3vec xyz))
                    (setf (gethash xyzi verts)
                          (add-vert! msh (vec:3add xy xyz))))))

    (loop for z of-type double-float in (list (- sz) sz)
          and zi of-type pos-int from 0
          do (loop for x of-type double-float in (list (- sx) sx)
                   and xi of-type pos-int from 0
                   do (loop for y of-type double-float in (list (- sy) sy)
                            and yi of-type pos-int from 0
                            do (funcall addvert (list xi yi zi)
                                                (vec:3vec x y z)))))
   (loop for poly in '(((0 0 0) (1 0 0) (1 1 0)) ((0 0 0) (0 1 0) (1 1 0))
                       ((0 0 1) (1 0 1) (1 1 1)) ((0 0 1) (0 1 1) (1 1 1))
                       ((0 0 1) (1 0 1) (1 0 0)) ((0 0 1) (0 0 0) (1 0 0))
                       ((1 0 1) (1 1 1) (1 1 0)) ((1 0 1) (1 0 0) (1 1 0))
                       ((0 0 1) (0 1 1) (0 1 0)) ((0 0 1) (0 0 0) (0 1 0))
                       ((0 1 1) (1 1 1) (1 1 0)) ((0 1 1) (0 1 0) (1 1 0)))
         collect (add-polygon! msh (funcall gp poly)))))


(defun add-open-box! (msh s h &key (xy vec:*3zero*) (rot 0d0)
                              &aux (left s) (right (- s)))
  (declare (mesh msh) (vec:3vec xy) (double-float s h rot left right))
  (let ((inds (list)))
    (labels ((indget (ii) (mapcar (lambda (i) (nth i inds)) ii)))
      (loop for v in (vec:lrot (list (vec:vec left right) (vec:vec right right)
                                     (vec:vec right left) (vec:vec left left))
                               rot :xy vec:*zero*)
            do (vec:with-xy (v x y)
                 (push (add-vert! msh
                         (vec:3add xy (vec:3vec x y h))) inds)
                 (push (add-vert! msh
                         (vec:3add xy (vec:3vec x y (- h)))) inds)))
      (add-polygons! msh
        (mapcar #'indget '((0 1 2) (1 2 3) (2 3 5) (2 4 5)
                           (6 4 5) (6 7 5) (0 6 7) (0 1 7)))))))


(defun add-polyhedra! (msh verts)
  (declare (mesh msh) (list verts))
  (let ((inds (add-verts! msh verts)))
    (labels ((indget (ii) (mapcar (lambda (i) (nth i inds)) ii)))
      (add-polygons! msh
        (mapcar #'indget '((0 1 2) (1 2 3) (0 2 3) (0 1 3)))))))


(defun add-rect! (msh &key (sx 1d0) (sy 1d0) (xy vec:*3zero*) (rot 0d0))
  (declare (mesh msh) (vec:3vec xy) (double-float sx sy rot))
  (let ((inds (list)))
    (labels ((indget (ii) (mapcar (lambda (i) (nth i inds)) ii)))
      (loop for v in (vec:lrot (list (vec:vec sx sy) (vec:vec (- sx) sy)
                                     (vec:vec (- sx) (- sy)) (vec:vec sx (- sy)))
                                rot :xy vec:*zero*)
            do (vec:with-xy (v x y)
                 (push (add-vert! msh
                         (vec:3add xy (vec:3vec x y 0d0))) inds)))
      (add-polygons! msh (mapcar #'indget '((0 1 2) (0 2 3)))))))

