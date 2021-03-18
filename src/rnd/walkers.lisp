
(in-package :rnd)


(defun get-lin-walk (&key (x 0d0))
  (declare #.*opt-settings* (double-float x))
  "linear random walker"
  (lambda (s) (declare (double-float s))
    (incf x (rnd* s))))


(defun get-lin-walk* (&key (x 0d0))
  (declare #.*opt-settings* (double-float x))
  "linear random walker limited to (0 1)"
  (lambda (s) (declare (double-float s))
    (setf x (math:dmod x (rnd* s) 1d0))))


(defun get-acc-lin-walk (&key (x 0d0) (a 0d0))
  (declare #.*opt-settings* (double-float x a))
  "accelerated linear random walker"
  (lambda (s) (declare (double-float s))
    (incf x (incf a (rnd* s)))))


(defun get-acc-lin-walk* (&key (x 0d0) (a 0d0))
  (declare #.*opt-settings* (double-float x a))
  "accelerated linear random walker limited to (0 1)"
  (lambda (s) (declare (double-float s))
    (setf x (math:dmod x (incf a (rnd* s)) 1d0))))


(defun get-circ-walk (&key (xy vec:*zero*))
  (declare #.*opt-settings* (vec:vec xy))
  "random 2d walker"
  (lambda (s) (declare (double-float s))
    (setf xy (vec:add xy (in-circ s)))))


(defun get-acc-circ-walk (&key (xy vec:*zero*) (a vec:*zero*))
  (declare #.*opt-settings* (vec:vec xy a))
  "random accelerated 2d walker"
  (lambda (s) (declare (double-float s))
    (setf xy (vec:add xy (setf a (vec:add a (in-circ s)))))))


; 3D

(defun 3get-sphere-walk (&key (xy vec:*3zero*))
  (declare #.*opt-settings* (vec:3vec xy))
  "random 3d walker"
  (lambda (s) (declare (double-float s))
    (setf xy (vec:3add xy (3in-sphere :rad s)))))


(defun 3get-acc-sphere-walk (&key (xy vec:*3zero*) (a vec:*3zero*))
  (declare #.*opt-settings* (vec:3vec xy a))
  "random accelerated 3d walker"
  (lambda (s) (declare (double-float s))
    (setf xy (vec:3add xy (setf a (vec:3add a (3in-sphere :rad s)))))))

