
(in-package :point-cloud)

(declaim (double-float *eps*))
(defparameter *eps* 1d-14)

(deftype pos-double () `(double-float 0d0 *))


(defun -point-bbox (pt rad)
  (declare #.*opt-settings* (vec:3vec pt) (double-float rad))
  (list (vec:3ssub pt rad) (vec:3sadd pt rad)))


(declaim (inline -make-objects))
(defun -make-objects (ptc rad vertfx)
  (declare #.*opt-settings* (point-cloud ptc) (function vertfx))
  "objects have the form (i pt mi ma)"
  (loop for i from 0 below (get-num-points ptc)
        collect (let ((pt (funcall vertfx i)))
                  (declare (vec:3vec pt))
                  (nconc (list i pt) (-point-bbox pt rad)))))


(declaim (inline -spherefx))
(defun -spherefx (pt rad2 org l)
  (declare #.*opt-settings* (vec:3vec pt org l) (pos-double rad2))
  (let* ((oc (vec:3sub org pt))
         (b (vec:3dot oc l))
         (y (- (the double-float (vec:3dot oc oc)) rad2)))
    (declare (vec:3vec oc) (double-float b y))
    (when (and (> y 0d0) (> b 0d0)) (return-from -spherefx -1d0))

    (let ((discr (- (* b b) y)))
      (declare (double-float discr))
      (when (<= discr 0d0) (return-from -spherefx -1d0))

      (let ((tt (- (+ b (the pos-double (sqrt (the pos-double discr)))))))
        (declare (double-float tt))
        (when (< tt 0d0) (return-from -spherefx -1d0))
        tt))))

(declaim (inline -make-spherefx))
(defun -make-spherefx (pt rad2)
  (declare #.*opt-settings* (vec:3vec pt) (pos-double rad2))
  (lambda (org l) (declare #.*opt-settings* (inline)) (-spherefx pt rad2 org l)))


(defun make-bvh (ptc &key rad (vertfx (make-point-getter ptc)) (num 5)
                              verbose &aux (rad2 (* rad rad)))
  (declare #.*opt-settings* (point-cloud ptc) (function vertfx)
                            (pos-int num) (pos-double rad rad2) (boolean verbose))
  (labels
    ((leaf-info (i pt)
      (declare (pos-int i) (vec:3vec pt))
      (list i pt (-make-spherefx pt rad2)))

     (build (root objs)
      (declare (bvh-utils::bvh-node root))
      (destructuring-bind (mi ma)
        (bvh-utils::-bbox (alexandria:flatten (mapcar #'cddr objs)))
        (declare (vec:3vec mi ma))
        (setf (bvh-utils::bvh-node-mi root) mi
              (bvh-utils::bvh-node-ma root) ma))

      (when (<= (length objs) num) (setf (bvh-utils::bvh-node-leaves root)
                                         (loop for (i pt mi ma) in objs
                                               collect (leaf-info i pt)))
            (return-from build))

      (setf objs (bvh-utils::-axissort objs))

      (let ((mid (ceiling (length objs) 2))
            (l (bvh-utils::-make-bvh-node))
            (r (bvh-utils::-make-bvh-node)))
        (declare (bvh-utils::bvh-node l r) (pos-int mid))
        (setf (bvh-utils::bvh-node-l root) l
              (bvh-utils::bvh-node-r root) r)
        (build l (subseq objs 0 mid))
        (build r (subseq objs mid)))))

    (let ((root (bvh-utils::-make-bvh-node)))
      (build root (-make-objects ptc rad vertfx))
      (when verbose (format t "~a~%" (bvh-utils::node-bbox-info root)))
      root)))


(defun make-bvh-raycaster (bvh)
  (declare #.*opt-settings* (bvh-utils::bvh-node bvh))
  "
  make raycaster based on the bvh structure.
  returns closest hit.
  "
  (labels
    ((recursive-raycast (root org l &key bfx hit)
      (declare #.*opt-settings* (inline) (bvh-utils:bvh-node root)
               (vec:3vec org l) (function bfx) (bvh-utils:bvh-result hit))

      (unless (funcall bfx (bvh-utils:bvh-node-mi root)
                           (bvh-utils:bvh-node-ma root))
              (return-from recursive-raycast))

      (let ((leaves (bvh-utils:bvh-node-leaves root)))
        (when leaves
              (loop for (i pt spherefx) of-type (fixnum vec:3vec function) in leaves
                    do (let ((s (funcall (the function spherefx) org l)))
                         (declare (double-float s))
                         (when (> s *eps*)
                               (bvh-utils:update-bvh-result hit s i pt))))
              (return-from recursive-raycast)))

      (when (bvh-utils:bvh-node-l root)
            (recursive-raycast (bvh-utils:bvh-node-l root) org l :bfx bfx :hit hit))
      (when (bvh-utils:bvh-node-r root)
            (recursive-raycast (bvh-utils:bvh-node-r root) org l :bfx bfx :hit hit)))

     (do-raycast (line)
       (declare #.*opt-settings* (inline) (list line))
       (let* ((org (first line))
              (ll (apply #'vec:3isub line))
              (l (vec:3norm ll))
              (bfx (bvh-utils:make-line-bbox-test org ll))
              (hit (bvh-utils:make-bvh-result)))
         (recursive-raycast bvh org l :bfx bfx :hit hit)
         hit)))

    #'do-raycast))

