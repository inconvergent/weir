
(in-package :bvh)

(declaim (double-float *eps* *inveps*))
(defparameter *eps* 1d-14)
(defparameter *inveps* (/ *eps*))


(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))


(declaim (inline -make-node node-leaves node-r node-l node-mi node-ma))
(defstruct (node (:constructor -make-node))
  (l nil :read-only nil)
  (r nil :read-only nil)
  (leaves nil :type list :read-only nil)
  (mi vec:*3zero* :type vec:3vec :read-only nil)
  (ma vec:*3zero* :type vec:3vec :read-only nil))

(weir-utils:define-struct-load-form node)

(declaim (inline -make-bch bvh-root bvh-normals))
(defstruct (bvh (:constructor -make-bvh))
  (root nil :type node :read-only t)
  (bt :none :type symbol :read-only t)
  (normals nil :read-only nil))

(weir-utils:define-struct-load-form bvh)


(declaim (inline -bbox))
(defun -bbox (pts)
  (declare #.*opt-settings* (list pts))
  (loop for pt of-type vec:3vec in pts
        minimizing (vec:3vec-x pt) into minx of-type double-float
        minimizing (vec:3vec-y pt) into miny of-type double-float
        minimizing (vec:3vec-z pt) into minz of-type double-float
        maximizing (vec:3vec-x pt) into maxx of-type double-float
        maximizing (vec:3vec-y pt) into maxy of-type double-float
        maximizing (vec:3vec-z pt) into maxz of-type double-float
        finally (return (list (vec:3vec minx miny minz)
                              (vec:3vec maxx maxy maxz)))))


(declaim (inline -longaxis))
(defun -longaxis (objs)
  (declare #.*opt-settings* (list objs))
  (destructuring-bind (mi ma) (-bbox (alexandria:flatten (map 'list #'cdr objs)))
    (declare (vec:3vec mi ma))
    (let ((dx (- (vec:3vec-x ma) (vec:3vec-x mi)))
          (dy (- (vec:3vec-y ma) (vec:3vec-y mi)))
          (dz (- (vec:3vec-z ma) (vec:3vec-z mi))))
      (declare (double-float dx dy dz))
      (cond ((and (>= dx dy) (>= dx dz)) #'vec:3vec-x)
            ((and (>= dy dx) (>= dy dz)) #'vec:3vec-y)
            (t #'vec:3vec-z)))))


(declaim (inline -axissort))
(defun -axissort (objs)
  (declare #.*opt-settings* (list objs))
  (let ((axisfx (-longaxis objs)))
    (declare (function axisfx))
    ; sort by bbox min: (second o)
    (sort objs #'< :key (lambda (o) (declare #.*opt-settings* (list o))
                          (the double-float (funcall axisfx (second o)))))))


(defun make (all-objs leaffx &key bt (num 5) verbose)
  (declare #.*opt-settings* (list all-objs) (function leaffx) (pos-int num)
                            (symbol bt))
  (labels
    ((build (root objs)
      (declare (node root))
      (destructuring-bind (mi ma)
        (-bbox (alexandria:flatten (map 'list #'cdr objs)))
        (declare (vec:3vec mi ma))
        (setf (node-mi root) mi (node-ma root) ma))

      (when (<= (length objs) num)
            (setf (node-leaves root) (loop for (i mi ma) in objs
                                           collect (funcall leaffx i)))
            (return-from build))

      (setf objs (-axissort objs))

      (let ((mid (ceiling (length objs) 2))
            (l (-make-node))
            (r (-make-node)))
        (declare (node l r) (pos-int mid))
        (setf (node-l root) l (node-r root) r)
        (build l (subseq objs 0 mid))
        (build r (subseq objs mid)))))

    (let* ((root (-make-node))
           (res (-make-bvh :root root :bt bt)))
      (build root all-objs)
      res)))

(declaim (inline -select-bound))
(defun -select-bound (org invl mi ma sig axis)
  (declare #.*opt-settings* (vec:3vec org invl mi ma) (boolean sig)
                            (function axis))
  (let ((invl (funcall axis invl))
        (org (funcall axis org)))
    (declare (double-float invl org))
    (if sig
        (values (* invl (the double-float (- (the double-float (funcall axis ma)) org)))
                (* invl (the double-float (- (the double-float (funcall axis mi)) org))))
        (values (* invl (the double-float (- (the double-float (funcall axis mi)) org)))
                (* invl (the double-float (- (the double-float (funcall axis ma)) org)))))))

;Brian Smits. Efficient bounding box intersection. Ray tracing news, 15(1), 2002.
;http://people.csail.mit.edu/amy/papers/box-jgt.pdf
(declaim (inline -bbox-test))
(defun -bbox-test (org invl sigx sigy sigz mi ma)
  (declare #.*opt-settings* (vec:3vec org invl mi ma)
                            (boolean sigx sigy sigz))

  (multiple-value-bind (tmin tmax)
    (-select-bound org invl mi ma sigx #'vec:3vec-x)
    (declare (double-float tmin tmax))

    (multiple-value-bind (tymin tymax)
      (-select-bound org invl mi ma sigy #'vec:3vec-y)
      (declare (double-float tymin tymax))

      (when (or (> tmin tymax) (> tymin tymax))
            (return-from -bbox-test nil))
      (when (> tymin tmin) (setf tmin tymin))
      (when (< tymax tmax) (setf tmax tymax))

      (multiple-value-bind (tzmin tzmax)
        (-select-bound org invl mi ma sigz #'vec:3vec-z)
        (declare (double-float tzmin tzmax))

        (when (or (> tmin tzmax) (> tzmin tmax))
              (return-from -bbox-test nil))
        (when (> tzmin tmin) (setf tmin tzmin))
        (when (< tzmax tmax) (setf tmax tzmax))
        (and (< tmin 1d0) (> tmax 0d0))))))


(declaim (inline -eps-div))
(defun -eps-div (x)
  (declare #.*opt-settings* (double-float x))
  (if (or (> x *eps*) (< x (- *eps*)))
      (/ 1d0 x)
      (/ 1d0 *inveps*)))

(declaim (inline -eps-inv))
(defun -eps-inv (l)
  (declare #.*opt-settings* (vec:3vec l))
  (vec:3vec (-eps-div (vec:3vec-x l))
            (-eps-div (vec:3vec-y l))
            (-eps-div (vec:3vec-z l))))

(defun make-line-bbox-test (org l)
  (declare #.*opt-settings* (vec:3vec org l))
  (let* ((invl (-eps-inv l))
         (sigx (< (vec:3vec-x invl) 0d0))
         (sigy (< (vec:3vec-y invl) 0d0))
         (sigz (< (vec:3vec-z invl) 0d0)))
    (declare (boolean sigx sigy sigz) (vec:3vec invl))
    (lambda (mi ma) (declare #.*opt-settings* (vec:3vec mi ma))
      (-bbox-test org invl sigx sigy sigz mi ma))))


(defun node-bbox-info (root)
  (unless root (return-from node-bbox-info nil))
  (let ((mi (node-mi root))
        (ma (node-ma root)))
    (format nil "bbox~%  x:~a ~a~%  y:~a ~a~%  z:~a ~a~%"
            (numshow (vec:3vec-x mi)) (numshow (vec:3vec-x ma))
            (numshow (vec:3vec-y mi)) (numshow (vec:3vec-y ma))
            (numshow (vec:3vec-z mi)) (numshow (vec:3vec-z ma)))))

