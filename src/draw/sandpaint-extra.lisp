(in-package :sandpaint)


(defun copy-rgba-array-to-from (target source size)
  (declare #.*opt-settings*
           (type (simple-array double-float) target source)
           (fixnum size))
  (loop for i of-type fixnum from 0 below (* size size 4)
        do (setf (aref target i) (the double-float (aref source i)))))


(defun copy-scale-rgba-array-to-from (target source scale size)
  (declare #.*opt-settings*
           (type (simple-array double-float) target source)
           (fixnum size))
  (loop for i of-type fixnum from 0 below (* size size 4)
        do (if (<= (aref scale i) 0)
               (setf (aref target i) (aref source i))
               (setf (aref target i) (/ (aref source i) (aref scale i))))))


; TODO: improve/move
(declaim (inline -rndflip))
(defun -rndflip (a)
  (+ a (rnd:rnd* 0.05d0)))

(defun chromatic-aberration (sand center &key (s 1d0) (noise 1d0))
  (declare (double-float s noise))
  (with-struct (sandpaint- size vals indfx) sand
    (declare (pos-int size) (function indfx))
    (let ((new-vals (make-rgba-array size)))

      (labels ((-offset-rgba (xi yi nxy channel)
                (let ((rx (round (-rndflip (vec::vec-x nxy))))
                      (ry (round (-rndflip (vec::vec-y nxy)))))
                  (if (and (>= rx 0) (< rx size)
                           (>= ry 0) (< ry size))
                      (setf (aref new-vals (funcall indfx rx ry channel))
                            (the double-float
                                 (aref vals (funcall indfx xi yi channel))))))))

        (copy-rgba-array-to-from new-vals vals size)
        (-square-loop (x y size)
          (let* ((xy (vec:vec (coerce x 'double-float)
                              (coerce y 'double-float)))
                 (dx (vec:smult (vec:sub (rnd:in-circ noise :xy xy) center) (/ s))))
            (-offset-rgba x y (vec:add xy dx) 0)
            (-offset-rgba x y (vec:sub xy dx) 2))))

      (copy-rgba-array-to-from vals new-vals size))))

