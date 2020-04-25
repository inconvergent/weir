
(in-package :pigment)

; these functions ignores the pigment alpha channel. intended for use in eg.
; raytracing, or if we know that alpha is always 1.

(declaim (inline -clamp))
(defun -clamp (v)
  (declare #.*opt-settings* (double-float v))
  (min 1d0 (the double-float (max 0d0 v))))


(declaim (inline non-a-scale))
(defun non-a-scale (c s)
  (declare #.*opt-settings* (rgba c) (double-float s))
  (-make-rgba (* (rgba-r c) s) (* (rgba-g c) s)
              (* (rgba-b c) s) 1d0))

(declaim (inline non-a-scale!))
(defun non-a-scale! (c s)
  (declare #.*opt-settings* (rgba c) (double-float s))
  (setf (rgba-r c) (* (rgba-r c) s) (rgba-g c) (* (rgba-g c) s)
        (rgba-b c) (* (rgba-b c) s))
  c)


(declaim (inline non-a-scale-add!))
(defun non-a-scale-add! (v q s)
  (declare #.*opt-settings* (rgba v q) (double-float s))
  (setf (rgba-r v) (+ (rgba-r v) (* (rgba-r q) s))
        (rgba-g v) (+ (rgba-g v) (* (rgba-g q) s))
        (rgba-b v) (+ (rgba-b v) (* (rgba-b q) s)))
  v)


(declaim (inline non-a-add))
(defun non-a-add (v q &key (a 1d0))
  (declare #.*opt-settings* (rgba v q) (double-float a))
  (-make-rgba (+ (rgba-r v) (rgba-r q)) (+ (rgba-g v) (rgba-g q))
              (+ (rgba-b v) (rgba-b q)) a))

(declaim (inline non-a-add!))
(defun non-a-add! (v q)
  (declare #.*opt-settings* (rgba v q))
  (setf (rgba-r v) (+ (rgba-r v) (rgba-r q))
        (rgba-g v) (+ (rgba-g v) (rgba-g q))
        (rgba-b v) (+ (rgba-b v) (rgba-b q)))
  v)

(declaim (inline non-a-mult))
(defun non-a-mult (v q &key (a 1d0))
  (declare #.*opt-settings* (rgba v q) (double-float a))
  (-make-rgba (* (rgba-r v) (rgba-r q)) (* (rgba-g v) (rgba-g q))
              (* (rgba-b v) (rgba-b q)) a))

(declaim (inline non-a-mult!))
(defun non-a-mult! (v q)
  (declare #.*opt-settings* (rgba v q))
  (setf (rgba-r v) (* (rgba-r v) (rgba-r q))
        (rgba-g v) (* (rgba-g v) (rgba-g q))
        (rgba-b v) (* (rgba-b v) (rgba-b q)))
  v)


(declaim (inline non-a-clamp))
(defun non-a-clamp (c)
  (declare #.*opt-settings* (rgba c))
  (-make-rgba (-clamp (rgba-r c)) (-clamp (rgba-g c))
              (-clamp (rgba-b c)) 1d0))

(declaim (inline non-a-clamp!))
(defun non-a-clamp! (c)
  (declare #.*opt-settings* (rgba c))
  (setf (rgba-r c) (-clamp (rgba-r c))
        (rgba-g c) (-clamp (rgba-g c))
        (rgba-b c) (-clamp (rgba-b c)))
  c)

