
(in-package :mesh)

(declaim (double-float PII))


(declaim (inline -make-objects))
(defun -make-objects (msh vertfx)
  (declare #.*opt-settings* (mesh msh) (function vertfx))
  (loop for poly of-type list in (get-all-polygons msh)
        collect (let ((vv (funcall vertfx poly)))
                  (nconc (list poly vv) (bvh-utils::-bbox vv)))))

(defun make-bvh (msh &key vertfx (num 5) verbose)
  (declare #.*opt-settings* (mesh msh) (function vertfx) (pos-int num)
                            (boolean verbose))
  (labels
    ((leaf-info (poly vv)
      (declare (list poly vv))
      (list poly (apply #'vec::3make-polyx vv)
                 (apply #'-poly-normal vv)))

     (build (root objs)
      (declare (bvh-utils::bvh-node root))
      (destructuring-bind (mi ma)
        (bvh-utils::-bbox (alexandria:flatten (mapcar #'cddr objs)))
        (declare (vec:3vec mi ma))
        (setf (bvh-utils::bvh-node-mi root) mi (bvh-utils::bvh-node-ma root) ma))

      (when (<= (length objs) num)
            (setf (bvh-utils::bvh-node-leaves root)
                  (loop for (poly vv mi ma) in objs
                        collect (leaf-info poly vv)))
            (return-from build))

      (setf objs (bvh-utils::-axissort objs))

      (let ((mid (ceiling (length objs) 2))
            (l (bvh-utils::-make-bvh-node))
            (r (bvh-utils::-make-bvh-node)))
        (declare (bvh-utils::bvh-node l r) (pos-int mid))
        (setf (bvh-utils::bvh-node-l root) l (bvh-utils::bvh-node-r root) r)
        (build l (subseq objs 0 mid))
        (build r (subseq objs mid)))))

    (let ((root (bvh-utils::-make-bvh-node)))
      (build root (-make-objects msh vertfx))
      (when verbose (format t "~a~%" (bvh-utils::node-bbox-info root)))
      root)))


(defun make-bvh-raycaster (bvh &key (resfx #'first))
  (declare #.*opt-settings* (bvh-utils::bvh-node bvh) (function resfx))
  "
  make raycaster based on the bvh structure.
  "
  (labels
    ((recursive-raycast (root l org
                         &key (skip *nilpoly*) boundfx hits)
      (declare #.*opt-settings* (bvh-utils::bvh-node root) (vec:3vec org l)
                                (function boundfx) (vector hits))

      (unless (funcall boundfx (bvh-utils::bvh-node-mi root)
                               (bvh-utils::bvh-node-ma root))
              (return-from recursive-raycast))

      (let ((leaves (bvh-utils::bvh-node-leaves root)))
        (when leaves
              (loop for (poly polyfx normal) in leaves
                    do (multiple-value-bind (isect s p)
                         (funcall (the function polyfx) org l)
                         (when (and isect (not (equal skip poly)))
                               (vextend (list s p poly normal) hits))))
              (return-from recursive-raycast)))

      (when (bvh-utils::bvh-node-l root)
            (recursive-raycast (bvh-utils::bvh-node-l root) l org
                               :skip skip :boundfx boundfx :hits hits))
      (when (bvh-utils::bvh-node-r root)
            (recursive-raycast (bvh-utils::bvh-node-r root) l org
                               :skip skip :boundfx boundfx :hits hits)))

     (do-raycast (line &key (skip *nilpoly*))
       (declare (list line))
       (let* ((org (first line))
              (l (apply #'vec:3isub line))
              (boundfx (bvh-utils::make-line-bbox-test org l))
              (hits (make-adjustable-vector :type 'vec:3vec)))
         (recursive-raycast bvh l org :skip skip :boundfx boundfx :hits hits)
         (funcall resfx (sort (to-list hits) #'< :key #'first)))))

    #'do-raycast))

