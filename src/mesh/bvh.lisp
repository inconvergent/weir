
(in-package :mesh)

(declaim (double-float *eps*))
(defparameter *eps* 1d-14)


(declaim (inline -make-objects-normals))
(defun -make-objects-normals (msh vertfx)
  (declare #.*opt-settings* (mesh msh) (function vertfx))
  (loop with normals = (make-hash-table :test #'equal)
        with objs = (list)
        for poly of-type list in (get-all-polygons msh)
        do (let ((vv (funcall vertfx poly)))
             (push (nconc (list poly) (bvh::-bbox vv)) objs)
             (setf (gethash poly normals) (apply #'-poly-normal vv)))
        finally (return (values objs normals))))



(defun make-bvh (msh &key vertfx (num 5))
  (declare #.*opt-settings* (mesh msh) (function vertfx) (pos-int num))
  (multiple-value-bind (objs normals) (-make-objects-normals msh vertfx)
    (let ((bvh (bvh::make objs
                          (lambda (poly)
                            (declare #.*opt-settings* (list poly))
                            (list poly (apply #'vec::3make-polyx
                                              (funcall vertfx poly))))
                          :num num
                          :bt :mesh)))
      (setf (bvh:bvh-normals bvh) normals)
      bvh)))


(declaim (inline make-bvhres bvhres-s bvhres-i))
(defstruct (bvhres)
  (i *nilpoly* :type list :read-only nil)
  (s 900000d0 :type double-float :read-only nil)
  (pt vec:*3zero* :type vec:3vec :read-only nil)
  (n vec:*3zero* :type vec:3vec :read-only nil))

(weir-utils:define-struct-load-form bvhres)

(declaim (inline -update-result))
(defun -update-result (res s i pt)
  (declare #.*opt-settings* (bvhres res) (list i) (double-float s) (vec:3vec))
  (when (< s (bvhres-s res))
        (setf (bvhres-s res) s (bvhres-i res) i (bvhres-pt res) pt)
        nil))

(defun make-raycaster (bvh &key)
  (declare #.*opt-settings* (bvh:bvh bvh))
  "
  make raycaster based on the bvh structure. returns closest hit.
  "
  (labels
    ((recursive-raycast (root org ll &key (skip *nilpoly*) bfx res)
      (declare #.*opt-settings* (inline) (bvh:node root)
               (vec:3vec org ll) (function bfx) (bvhres res))

      (unless (funcall bfx (bvh:node-mi root) (bvh:node-ma root))
              (return-from recursive-raycast))

      (let ((leaves (bvh:node-leaves root)))
        (when leaves
              (loop for (i leaffx) of-type (list function) in leaves
                    ; TODO: check this more carefully
                    if (not (equal i skip))
                    do (multiple-value-bind (isect s pt)
                         (funcall (the function leaffx) org ll)
                         (when (and isect (> s *eps*) )
                               (-update-result res s i pt))))
              (return-from recursive-raycast)))

      (when (bvh:node-l root)
            (recursive-raycast (bvh:node-l root) org ll :bfx bfx :res res))
      (when (bvh:node-r root)
            (recursive-raycast (bvh:node-r root) org ll :bfx bfx :res res)))

     (do-raycast (line &key (skip *nilpoly*))
       (declare (list line))
       (let* ((org (first line))
              (ll (apply #'vec:3isub line))
              (res (make-bvhres)))
         (recursive-raycast (bvh:bvh-root bvh) org ll :skip skip
                            :bfx (bvh:make-line-bbox-test org ll)
                            :res res)
         (when (< (bvhres-s res) 900000d0)
               (setf (bvhres-n res)
                     (gethash (bvhres-i res) (bvh:bvh-normals bvh))))
         res)))

    #'do-raycast))

