
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


;https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
; slightly simpler version compared to the implementation in vec/3vec.lisp
(declaim (inline -polyx))
(defun -polyx (v0 e1 e2 org l)
  (declare #.*opt-settings* (vec:3vec org l v0 e1 e2))
  (let* ((h (vec:3cross l e2))
         (a (vec:3dot e1 h)))
    (declare (vec:3vec h) (double-float a))

  ; parallel
  (when (< (abs a) *eps*) (return-from -polyx (values nil 0d0)))

  (let* ((f (/ a))
         (s (vec:3sub org v0))
         (u (* f (vec:3dot s h))))
    (declare (vec:3vec s) (double-float f u))

    (when (or (> u 1d0) (< u 0d0)) (return-from -polyx (values nil 0d0)))

    (let* ((q (vec:3cross! s e1))
           (v (* f (vec:3dot l q))))
      (declare (vec:3vec q) (double-float v))
      (when (or (< v 0d0) (> (+ u v) 1d0)) (return-from -polyx (values nil 0d0)))

      (let ((tt (* f (vec:3dot e2 q))))
        (declare (double-float tt))
        (if (> 1d0 tt *eps*) (values t tt)
                             (values nil 0d0)))))))

(declaim (inline make-polyx))
(defun make-polyx (v0 v1 v2)
  (declare #.*opt-settings* (vec:3vec v0 v1 v2))
  (let ((e1 (vec:3sub v1 v0))
        (e2 (vec:3sub v2 v0)))
    (declare (vec:3vec e1 e2))
    (lambda (org l) (declare #.*opt-settings* (vec:3vec org l))
      (-polyx v0 e1 e2 org l))))


(defun make-bvh (msh &key vertfx (num 5))
  (declare #.*opt-settings* (mesh msh) (function vertfx) (pos-int num))
  (multiple-value-bind (objs normals) (-make-objects-normals msh vertfx)
    (let ((bvh (bvh::make objs
                          (lambda (poly)
                            (declare #.*opt-settings* (list poly))
                            (list poly (apply #'make-polyx
                                              (funcall vertfx poly))))
                          :num num
                          :bt :mesh)))
      (setf (bvh:bvh-normals bvh) normals)
      bvh)))


(declaim (inline make-bvhres bvhres-s bvhres-i bvhres-n bvhres-pt))
(defstruct (bvhres)
  (i *nilpoly* :type list :read-only nil)
  (s 900000d0 :type double-float :read-only nil)
  (pt vec:*3zero* :type vec:3vec :read-only nil)
  (n vec:*3zero* :type vec:3vec :read-only nil))

(weir-utils:define-struct-load-form bvhres)

(declaim (inline -update-result))
(defun -update-result (res s i pt)
  (declare #.*opt-settings* (bvhres res) (list i) (double-float s))
  (when (< s (bvhres-s res))
        (setf (bvhres-s res) s (bvhres-i res) i (bvhres-pt res) pt))
  nil)

;(declaim (inline -do-raycast))
(defun -do-raycast (root org ll bfx res)
  (declare #.*opt-settings* (bvh:node root) (vec:3vec org ll)
                            (function bfx) (bvhres res))
  (unless (funcall bfx (bvh:node-mi root) (bvh:node-ma root))
          (return-from -do-raycast))
  (let ((leaves (bvh:node-leaves root)))
    (declare (list leaves))
    (when leaves
          (loop for (i leaffx) of-type (list function) in leaves
                do (multiple-value-bind (isect s)
                     (funcall (the function leaffx) org ll)
                     (declare (boolean isect) (double-float s))
                     (when isect (-update-result res s i (vec:3from org ll s)))))
          (return-from -do-raycast)))
  (when (bvh:node-l root) (-do-raycast (bvh:node-l root) org ll bfx res))
  (when (bvh:node-r root) (-do-raycast (bvh:node-r root) org ll bfx res)))

(declaim (inline raycast))
(defun raycast (bvh line)
  (declare #.*opt-settings* (bvh:bvh bvh) (list line))
  (let ((org (first line))
        (ll (apply #'vec:3isub line))
        (res (make-bvhres)))
    (declare (vec:3vec org ll) (bvhres res))
    (-do-raycast (bvh:bvh-root bvh) org ll (bvh:make-line-bbox-test org ll) res)
    (when (< (bvhres-s res) 900000d0)
          (setf (bvhres-n res)
                (gethash (bvhres-i res) (bvh:bvh-normals bvh))))
    res))

