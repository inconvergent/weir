
(in-package :bvh)

(declaim (double-float *eps*))
(defparameter *eps* 1d-14)


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
      ;(cond ((and (> dx dy) (> dx dz)) #'vec:3vec-x)
      ;      ((and (> dy dx) (> dy dz)) #'vec:3vec-y)
      ;      (t #'vec:3vec-z))
      (second (first (sort `((,dx ,#'vec:3vec-x)
                             (,dy ,#'vec:3vec-y)
                             (,dz ,#'vec:3vec-z))
                           #'> :key #'first))))))


(declaim (inline -axissort))
(defun -axissort (objs)
  (declare #.*opt-settings* (list objs))
  (let ((axisfx (-longaxis objs)))
    (declare (function axisfx))
    ; sort by bbox min: (second o)
    ;(sort objs #'< :key (lambda (o) (declare #.*opt-settings* (list o))
    ;                      (the double-float (funcall axisfx (second o)))))
    (sort objs (lambda (a b) (declare (list a b))
                 (and (< (the double-float (funcall axisfx (second a)))
                         (the double-float (funcall axisfx (second b))))
                      (< (the double-float (funcall axisfx (third a)))
                         (the double-float (funcall axisfx (third b)))))))))


(defun make (all-objs leaffx &key bt (num 5))
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


(defmacro -select-bound ((org invl mi ma sig tmin tmax afx) &body body)
  (alexandria:with-gensyms (ai ao)
    `(let ((,ai (,afx ,invl))
           (,ao (,afx ,org)))
      (declare (double-float ,ai ,ao))
      (if ,sig (let ((,tmin (* ,ai (- (,afx ,mi) ,ao)))
                     (,tmax (* ,ai (- (,afx ,ma) ,ao))))
                 (declare (double-float ,tmin ,tmax))
                 (progn ,@body))
               (let ((,tmin (* ,ai (- (,afx ,ma) ,ao)))
                     (,tmax (* ,ai (- (,afx ,mi) ,ao))))
                 (declare (double-float ,tmin ,tmax))
                 (progn ,@body))))))

;Brian Smits. Efficient bounding box intersection. Ray tracing news, 15(1), 2002.
;http://people.csail.mit.edu/amy/papers/box-jgt.pdf
(declaim (inline -bbox-test))
(defun -bbox-test (org invl sigx sigy sigz mi ma)
  (declare #.*opt-settings* (vec:3vec org invl mi ma)
                            (boolean sigx sigy sigz))
  (-select-bound (org invl mi ma sigx tmin tmax vec:3vec-x)
    (-select-bound (org invl mi ma sigy tymin tymax vec:3vec-y)
      (when (or (> tmin tymax) (> tymin tmax)) (return-from -bbox-test nil))
      (when (> tymin tmin) (setf tmin tymin))
      (when (< tymax tmax) (setf tmax tymax))
      (-select-bound (org invl mi ma sigz tzmin tzmax vec:3vec-z)
        (when (or (> tmin tzmax) (> tzmin tmax)) (return-from -bbox-test nil))
        (when (> tzmin tmin) (setf tmin tzmin))
        (when (< tzmax tmax) (setf tmax tzmax))
        (and (< tmin 1d0) (> tmax 0d0))))))


(declaim (inline -eps-div))
(defun -eps-div (x)
  (declare #.*opt-settings* (double-float x))
  (if (or (> x *eps*) (< x (- *eps*))) (/ x) *eps*))

(declaim (inline -eps-inv))
(defun -eps-inv (l)
  (declare #.*opt-settings* (vec:3vec l))
  (vec:3vec (-eps-div (vec:3vec-x l))
            (-eps-div (vec:3vec-y l))
            (-eps-div (vec:3vec-z l))))

(defun make-line-bbox-test (org l)
  (declare #.*opt-settings* (vec:3vec org l))
  (let* ((invl (-eps-inv l))
         (sigx (<= 0d0 (vec:3vec-x invl)))
         (sigy (<= 0d0 (vec:3vec-y invl)))
         (sigz (<= 0d0 (vec:3vec-z invl))))
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

