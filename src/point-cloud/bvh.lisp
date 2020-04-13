
(in-package :point-cloud)

(declaim (double-float *eps*))
(defparameter *eps* 1d-14)

(deftype pos-double () `(double-float 0d0 *))

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))


(defun -point-bbox (pt rad)
  (declare #.*opt-settings* (vec:3vec pt) (double-float rad))
  (list (vec:3ssub pt rad) (vec:3sadd pt rad)))


(declaim (inline -make-objects))
(defun -make-objects (ptc rad ptfx)
  (declare #.*opt-settings* (point-cloud ptc) (function ptfx))
  "objects have the form (i pt mi ma)"
  (loop with n = (get-num-points ptc)
        ;with res = (make-array n :element-type 'list :adjustable nil)
        for i from 0 below n
        collect (nconc (list i) (-point-bbox (funcall ptfx i) rad))))


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
  (lambda (org l) (declare #.*opt-settings*) (-spherefx pt rad2 org l)))


(defun make-bvh (ptc &key rad (ptfx (make-point-getter ptc)) (num 5)
                     &aux (rad2 (* rad rad)))
  (declare #.*opt-settings* (point-cloud ptc) (function ptfx)
                            (pos-int num) (pos-double rad rad2))

  (bvh::make (-make-objects ptc rad ptfx)
             (lambda (i) (declare #.*opt-settings* (fixnum i))
               (list i (-make-spherefx (funcall ptfx i) rad2)))
             :num num
             :bt :point-cloud))


(declaim (inline make-bvhres bvhres-s bvhres-i))
(defstruct (bvhres)
  (i -1 :type fixnum :read-only nil)
  (s 900000d0 :type double-float :read-only nil))

(weir-utils:define-struct-load-form bvhres)


(declaim (inline -update-result))
(defun -update-result (res s i)
  (declare #.*opt-settings* (bvhres res) (fixnum i) (double-float s))
  (when (< s (bvhres-s res))
        (setf (bvhres-s res) s (bvhres-i res) i)
        nil))

(defun make-raycaster (bvh)
  (declare #.*opt-settings* (bvh:bvh bvh))
  "
  make raycaster based on the bvh structure.
  returns closest hit.
  "
  (labels
    ((recursive-raycast (root org l &key bfx res)
      (declare #.*opt-settings* (inline) (bvh:node root)
               (vec:3vec org l) (function bfx) (bvhres res))

      (unless (funcall bfx (bvh:node-mi root) (bvh:node-ma root))
              (return-from recursive-raycast))

      (let ((leaves (bvh:node-leaves root)))
        (when leaves
              (loop for (i leaffx) of-type (fixnum function) in leaves
                    do (let ((s (funcall (the function leaffx) org l)))
                         (declare (double-float s))
                         (when (> s *eps*) (-update-result res s i))))
              (return-from recursive-raycast)))

      (when (bvh:node-l root)
            (recursive-raycast (bvh:node-l root) org l :bfx bfx :res res))
      (when (bvh:node-r root)
            (recursive-raycast (bvh:node-r root) org l :bfx bfx :res res)))

     (do-raycast (line)
       (declare #.*opt-settings* (inline) (list line))
       (let* ((org (first line))
              (ll (apply #'vec:3isub line))
              (l (vec:3norm ll))
              (res (make-bvhres)))
         (recursive-raycast (bvh:bvh-root bvh) org l
                            :bfx (bvh:make-line-bbox-test org ll)
                            :res res)
         res)))

    #'do-raycast))

