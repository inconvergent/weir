
(in-package :pigment)


(declaim (inline non-a-scale))
(defun non-a-scale (c s)
  (declare #.*opt-settings* (rgba c) (double-float s))
  ;ignores alpha channel
  (make-rgba :r (* (rgba-r c) s) :g (* (rgba-g c) s)
             :b (* (rgba-b c) s) :a 1d0))

(declaim (inline non-a-scale!))
(defun non-a-scale! (c s)
  (declare #.*opt-settings* (rgba c) (double-float s))
  ;ignores alpha channel
  (setf (rgba-r c) (* (rgba-r c) s)
        (rgba-g c) (* (rgba-g c) s)
        (rgba-b c) (* (rgba-b c) s))
  c)


(declaim (inline non-a-scale-add!))
(defun non-a-scale-add! (v q s)
  (declare #.*opt-settings* (rgba v q) (double-float s))
  ;ignores alpha channel
  (setf (rgba-r v) (+ (rgba-r v) (* (rgba-r q) s))
        (rgba-g v) (+ (rgba-g v) (* (rgba-g q) s))
        (rgba-b v) (+ (rgba-b v) (* (rgba-b q) s)))
  v)


(declaim (inline non-a-add))
(defun non-a-add (v q &key (a 1d0))
  (declare #.*opt-settings* (rgba v q) (double-float a))
  ;ignores alpha channel
  (make-rgba :r (+ (rgba-r v) (rgba-r q))
             :g (+ (rgba-g v) (rgba-g q))
             :b (+ (rgba-b v) (rgba-b q)) :a a))

(declaim (inline non-a-add!))
(defun non-a-add! (v q)
  (declare #.*opt-settings* (rgba v q))
  ;ignores alpha channel
  (setf (rgba-r v) (+ (rgba-r v) (rgba-r q))
        (rgba-g v) (+ (rgba-g v) (rgba-g q))
        (rgba-b v) (+ (rgba-b v) (rgba-b q)))
  v)

