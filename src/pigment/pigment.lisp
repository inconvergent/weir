
(in-package :pigment)

"""
Colors are stored internally with premultiplied alpha.
Package was renamed from 'color' because of a package name collision.
"""

(defmacro with ((c r g b a) &body body)
  (alexandria:with-gensyms (c*)
    `(let* ((,c* ,c)
            (,r (rgba-r ,c*))
            (,g (rgba-g ,c*))
            (,b (rgba-b ,c*))
            (,a (rgba-a ,c*)))
      (declare (double-float ,r ,g ,b ,a))
      (progn ,@body))))


(defmacro -with ((c r g b a) &body body)
  (alexandria:with-gensyms (c*)
    `(let* ((,c* ,c)
            (,a (rgba-a ,c*))
            (,r (/ (rgba-r ,c*) ,a))
            (,g (/ (rgba-g ,c*) ,a))
            (,b (/ (rgba-b ,c*) ,a)))
      (declare (double-float ,r ,g ,b ,a))
      (progn ,@body))))


(declaim (inline make-rgba rgba-r rgba-g rgba-b rgba-a))
(defstruct (rgba)
  (r 0d0 :type double-float :read-only nil)
  (g 0d0 :type double-float :read-only nil)
  (b 0d0 :type double-float :read-only nil)
  (a 1d0 :type double-float :read-only nil))

(weir-utils:define-struct-load-form rgba)


(declaim (inline make))
(defun make (r g b &optional (a 1d0))
  (declare #.*opt-settings* (double-float r g b a))
  (make-rgba :r (* a r) :g (* a g) :b (* a b) :a a))

(declaim (inline copy))
(defun copy (c)
  (declare #.*opt-settings* (rgba c))
  (make-rgba :r (rgba-r c) :g (rgba-g c) :b (rgba-b c) :a (rgba-a c)))

(defun to-list (c)
  (declare #.*opt-settings* (rgba c))
  (let ((a (rgba-a c)))
    (list (/ (rgba-r c) a) (/ (rgba-g c) a) (/ (rgba-b c) a) a)))

(defun to-list* (c)
  (declare #.*opt-settings* (rgba c))
  (list (rgba-r c) (rgba-g c) (rgba-b c) (rgba-a c)))


(defun white (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 1d0 1d0 1d0 a))

(defun black (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 0d0 0d0 0d0 a))

(defun red (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 1d0 0d0 0d0 a))

(defun green (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 0d0 1d0 0d0 a))

(defun blue (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 0d0 0d0 1d0 a))

(defun mdark (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 0.3d0 0.3d0 0.3d0 a))

(defun dark (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 0.2d0 0.2d0 0.2d0 a))

(defun vdark (&optional (a 1d0))
  (declare #.*opt-settings* (double-float a))
  (make 0.1d0 0.1d0 0.1d0 a))

(defun gray (v &optional (a 1d0))
  (declare #.*opt-settings* (double-float v a))
  (make v v v a))

(defun transparent ()
  (declare #.*opt-settings*)
  (make 0d0 0d0 0d0 0d0))


(declaim (inline rgb))
(defun rgb (r g b &optional (a 1d0))
  (declare #.*opt-settings* (double-float r g b a))
  (make r g b a))


(declaim (inline scale))
(defun scale (c s)
  (declare #.*opt-settings* (rgba c) (double-float s))
  (make-rgba :r (* (rgba-r c) s)
             :g (* (rgba-g c) s)
             :b (* (rgba-b c) s)
             :a (* (rgba-a c) s)))

(declaim (inline scale!))
(defun scale! (c s)
  (declare #.*opt-settings* (rgba c) (double-float s))
  (setf (rgba-r c) (* (rgba-r c) s)
        (rgba-g c) (* (rgba-g c) s)
        (rgba-b c) (* (rgba-b c) s)
        (rgba-a c) (* (rgba-a c) s))
  c)


(declaim (inline safe-clamp))
(defun safe-clamp (c)
  "clamp between 0, a, since we use pre-mult alpha"
  (let ((a (rgba-a c)))
    (declare (double-float a))
    (make-rgba :r (min a (max 0d0 (rgba-r c)))
               :g (min a (max 0d0 (rgba-g c)))
               :b (min a (max 0d0 (rgba-b c)))
               :a a)
    c))

(declaim (inline safe-clamp!))
(defun safe-clamp! (c)
  "clamp between 0, a, since we use pre-mult alpha"
  (let ((a (rgba-a c)))
    (declare (double-float a))
    (setf (rgba-r c) (min a (max 0d0 (rgba-r c)))
          (rgba-g c) (min a (max 0d0 (rgba-g c)))
          (rgba-b c) (min a (max 0d0 (rgba-b c))))
    c))



