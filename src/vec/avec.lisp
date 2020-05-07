
(in-package :avec)

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))

(deftype pos-double () `(double-float 0d0 *))


(declaim (inline avec))
(defun avec (n &key (dim 2) (init 0d0))
  (make-array (* n dim) :initial-element init :element-type 'double-float))


(declaim (inline dst2))
(defun dst2 (a b u v)
  (declare #.*opt-settings*
           (type (simple-array double-float) a b) (pos-int u v))
  (let ((uu (* 2 u))
        (vv (* 2 v)))
    (declare (pos-int uu vv))
    (+ (expt (- (the double-float (aref a uu))
                (the double-float (aref b vv))) 2d0)
       (expt (- (the double-float (aref a (1+ (the pos-int uu))))
                (the double-float (aref b (1+ (the pos-int vv))))) 2d0))))

(declaim (inline dst))
(defun dst (a b u v)
  (declare #.*opt-settings*
           (type (simple-array double-float) a b) (pos-int u v))
  (sqrt (the pos-double (dst2 a b u v))))


(declaim (inline 3dst2))
(defun 3dst2 (a b u v)
  (declare #.*opt-settings*
           (type (simple-array double-float) a b) (pos-int u v))
  (let ((uu (* 3 u))
        (vv (* 3 v)))
    (declare (pos-int uu vv))
    (+ (expt (- (the double-float (aref a uu))
                (the double-float (aref b vv))) 2d0)
       (expt (- (the double-float (aref a (1+ (the pos-int uu))))
                (the double-float (aref b (1+ (the pos-int vv))))) 2d0)
       (expt (- (the double-float (aref a (+ 2 (the pos-int uu))))
                (the double-float (aref b (+ 2 (the pos-int vv))))) 2d0))))

(declaim (inline 3dst))
(defun 3dst (a b u v)
  (declare #.*opt-settings*
           (type (simple-array double-float) a b) (pos-int u v))
  (sqrt (the pos-double (3dst2 a b u v))))


(declaim (inline getv))
(defun getv (a i &aux (ii (* 2 i)))
  "get value of a[i] as v"
  (declare #.*opt-settings*
           (pos-int i ii) (type (simple-array double-float) a))
  (vec:vec (aref a ii) (aref a (1+ ii))))

(declaim (inline setv))
(defun setv (a i v &aux (ii (* 2 i)))
  "set value of a[i] to v. returns v"
  (declare #.*opt-settings*
           (vec:vec v) (pos-int i ii) (type (simple-array double-float) a))
  (setf (aref a ii) (the double-float (vec:vec-x v))
        (aref a (1+ ii)) (the double-float (vec:vec-y v)))
  v)


(declaim (inline 3getv))
(defun 3getv (a i &aux (ii (* 3 i)))
  "get value of a[i] as v"
  (declare #.*opt-settings* (pos-int i ii) (type (simple-array double-float) a))
  (vec:3vec (aref a ii) (aref a (1+ ii)) (aref a (+ 2 ii))))

(declaim (inline 3setv))
(defun 3setv (a i v &aux (ii (* 3 i)))
  "set value of a[i] to v. returns v"
  (declare #.*opt-settings*
           (vec:3vec v) (pos-int i ii) (type (simple-array double-float) a))
  (setf (aref a ii) (the double-float (vec:3vec-x v))
        (aref a (1+ ii)) (the double-float (vec:3vec-y v))
        (aref a (+ 2 ii)) (the double-float (vec:3vec-z v)))
  v)


; TODO: generalize these functions
(declaim (inline minmax*))
(defun minmax* (a inds)
  (declare #.*opt-settings* (list inds) (type (simple-array double-float) a))
  (loop with i of-type pos-int = 0
        for i* of-type pos-int in inds
        do (setf i (* i* 2))
        minimizing (aref a i) into minx of-type double-float
        maximizing (aref a i) into maxx of-type double-float
        minimizing (aref a (1+ i)) into miny of-type double-float
        maximizing (aref a (1+ i)) into maxy of-type double-float
        finally (return (values minx maxx miny maxy))))

(declaim (inline 3minmax*))
(defun 3minmax* (a inds)
  (declare #.*opt-settings* (list inds) (type (simple-array double-float) a))
  (loop with i of-type pos-int = 0
        for i* of-type pos-int in inds
        do (setf i (* i* 3))
        minimizing (aref a i) into minx of-type double-float
        maximizing (aref a i) into maxx of-type double-float
        minimizing (aref a (1+ i)) into miny of-type double-float
        maximizing (aref a (1+ i)) into maxy of-type double-float
        minimizing (aref a (+ i 2)) into minz of-type double-float
        maximizing (aref a (+ i 2)) into maxz of-type double-float
        finally (return (values minx maxx miny maxy minz maxz))))

(declaim (inline minmax))
(defun minmax (a num)
  (declare #.*opt-settings* (pos-int num) (type (simple-array double-float) a))
  (loop for i of-type pos-int from 0 below (* 2 num) by 2
        minimizing (aref a i) into minx of-type double-float
        maximizing (aref a i) into maxx of-type double-float
        minimizing (aref a (1+ i)) into miny of-type double-float
        maximizing (aref a (1+ i)) into maxy of-type double-float
        finally (return (values minx maxx miny maxy))))

(declaim (inline 3minmax))
(defun 3minmax (a num)
  (declare #.*opt-settings* (pos-int num) (type (simple-array double-float) a))
  (loop for i of-type pos-int from 0 below (* 3 num) by 3
        minimizing (aref a i) into minx of-type double-float
        maximizing (aref a i) into maxx of-type double-float
        minimizing (aref a (1+ i)) into miny of-type double-float
        maximizing (aref a (1+ i)) into maxy of-type double-float
        minimizing (aref a (+ i 2)) into minz of-type double-float
        maximizing (aref a (+ i 2)) into maxz of-type double-float
        finally (return (values minx maxx miny maxy minz maxz))))


(defmacro with-vec ((a ind x y) &body body)
  "
  assign values if a[ind] to (x y). after body is executed, the current
  value of (x y) will be written back to a[ind].
  "
  (declare (symbol x y))
  (alexandria:with-gensyms (i0 i1 res)
    `(let* ((,i0 (* 2 ,ind))
            (,i1 (+ ,i0 1))
            (,x (aref ,a ,i0))
            (,y (aref ,a ,i1)))
      (declare (pos-int ,i0 ,i1) (double-float ,x ,y))
      (let ((,res (progn ,@body)))
        (setf (aref ,a ,i0) ,x (aref ,a ,i1) ,y)
        ,res))))

(defmacro 3with-vec ((a ind x y z) &body body)
  "
  assign values if a[ind] to (x y z). after body is executed, the current
  value of (x y z) will be written back to a[ind].
  "
  (declare (symbol x y z))
  (alexandria:with-gensyms (i0 i1 i2 res)
    `(let* ((,i0 (the pos-int (* 3 (the pos-int ,ind))))
            (,i1 (+ ,i0 1))
            (,i2 (+ ,i0 2))
            (,x (aref ,a ,i0))
            (,y (aref ,a ,i1))
            (,z (aref ,a ,i2)))
      (declare (pos-int ,i0 ,i1 ,i2) (double-float ,x ,y ,z))
      (let ((,res (progn ,@body)))
        (setf (aref ,a ,i0) ,x (aref ,a ,i1) ,y (aref ,a ,i2) ,z)
        ,res))))

