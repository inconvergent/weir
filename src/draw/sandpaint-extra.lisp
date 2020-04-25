(in-package :sandpaint)


(declaim (inline -clamp))
(defun -clamp (v)
  (declare #.*opt-settings* (double-float v))
  (min 1d0 (the double-float (max 0d0 v))))


(declaim (inline sample))
(defun sample (sand xy &key (alpha 1d0))
  (-do-op (sand size vals indfx)
    (-inside-floor (size xy x y)
      (let* ((ind (funcall indfx x y))
             (a (aref vals (+ ind 3))))
        (declare (pos-int ind) (double-float a))
        (pigment:rgb (/ (aref vals ind) a)
                     (/ (aref vals (1+ ind)) a)
                     (/ (aref vals (+ ind 2)) a)
                     alpha)))))


(declaim (inline sample-bilin))
(defun sample-bilin (sand xy)
  (declare #.*opt-settings* (sandpaint sand) (vec:vec xy))
  (with-struct (sandpaint- vals size indfx) sand
    (multiple-value-bind (ix iy fx fy) (-floor-fract xy)
      (declare (fixnum ix iy) (double-float fx fy))
      (multiple-value-bind (s1 s2 s3 s4) (-fract-overlap fx fy)
        (declare (double-float s1 s2 s3 s4))
        (let* ((size* (- size 2))
               (ind (funcall indfx (min size* (max ix 0))
                                   (min size* (max iy 0)))))
          (declare (pos-int size* ind))
          (labels ((-sample-channel (i)
                  (+ (* s1 (aref vals i))
                     (* s2 (aref vals (+ i 4)))
                     (* s3 (aref vals (+ i (* size 4))))
                     (* s4 (aref vals (+ i (* size 4) 4))))))
          (pigment::-make-rgba (-sample-channel ind)
                               (-sample-channel (+ 1 ind))
                               (-sample-channel (+ 2 ind))
                               (-sample-channel (+ 3 ind)))))))))


(defun copy-rgba-array-to-from (target source size)
  (declare #.*opt-settings*
           (type (simple-array double-float) target source)
           (pos-int size))
  (loop for i of-type pos-int from 0 below (* size size 4)
        do (setf (aref target i) (the double-float (aref source i)))))


(defun copy-scale-rgba-array-to-from (target source scale size)
  (declare #.*opt-settings*
           (type (simple-array double-float) target source)
           (pos-int size))
  (loop for i of-type pos-int from 0 below (* size size 4)
        do (if (<= (aref scale i) 0)
               (setf (aref target i) (aref source i))
               (setf (aref target i) (/ (aref source i) (aref scale i))))))


(defun cafx-expt (mid s ps)
  (lambda (xy)
    (let* ((dx (vec:sub xy mid))
           (len (max 1d0 (vec:len dx)))
           (ex (/ 1d0 (+ 1 (exp (- (/ len s)))))))
      (vec:smult dx (* ps (/ ex s))))))

(defun cafx-lin (mid s)
  (lambda (xy) (vec:smult (vec:sub xy mid) (/ s))))

(defun chromatic-aberration (sand &key (cafx (cafx-expt (vec:rep 1000d0) 1000d0 2d0)))
  (declare (sandpaint sand) (function cafx))
  (with-struct (sandpaint- size vals indfx) sand
    (declare (pos-int size) (function indfx))
    (let ((new-vals (make-rgba-array size)))
      (copy-rgba-array-to-from new-vals vals size)
      (labels ((-offset-channel (xi yi channel val)
                (setf (aref new-vals (funcall indfx xi yi channel)) val)))

        (-square-loop (x y size)
          (let* ((xy (vec:vec (coerce x 'double-float) (coerce y 'double-float)))
                 (dx (funcall cafx xy)))
            (declare (vec:vec xy dx))
            (-offset-channel x y 0
              (pigment::rgba-r (sample-bilin sand (vec:sub xy dx))))
            (-offset-channel x y 2
              (pigment::rgba-b (sample-bilin sand (vec:add xy dx)))))))

      (copy-rgba-array-to-from vals new-vals size))))

(declaim (inline hsv))
(defun hsv (sand &key (h 0d0) (s 0d0) (v 0d0))
  (declare (sandpaint sand) (double-float h s v))
  (-do-op (sand size vals indfx)
    (-square-loop (x y size)
      (let ((ind (funcall indfx x y)))
        (declare (pos-int ind))
        (destructuring-bind (h* s* v* a)
          (pigment:as-hsv (pigment::-make-rgba
                            (aref vals ind) (aref vals (1+ ind))
                            (aref vals (+ ind 2)) (aref vals (+ ind 3))))
          (declare (double-float h* s* v* a))
          (set-pix sand x y (pigment:hsv (mod (+ h h*) 1d0) (-clamp (+ s s*))
                                         (-clamp (+ v v*)) a)))))))


(defun check-integrity (sand)
  (declare (sandpaint sand))
  (-do-op (sand size vals indfx)
    (-square-loop (x y size)
      (let* ((ind (funcall indfx x y))
             (r (aref vals ind))
             (g (aref vals (+ 1 ind)))
             (b (aref vals (+ 2 ind)))
             (a (aref vals (+ 3 ind))))
        (declare (pos-int ind) (double-float r g b a))
        (when (> a 1d0) (error "invalid alpha value, high"))
        (when (< a 0d0) (error "invalid alpha value, low"))
        (when (or (< r 0d0) (< g 0d0) (< b 0d0))
              (error "invalid rgb value, negative"))
        (when (or (> r a) (> g a) (> b a))
              (error "invalid rgb value, above alpha"))))))

