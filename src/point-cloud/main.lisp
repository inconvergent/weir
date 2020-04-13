
(in-package :point-cloud)

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))

(deftype 3vec-simple () `(simple-array vec:3vec))

(declaim (fixnum *nilpt*))
(defvar *nilpt* -1)


(defstruct (point-cloud (:constructor -make-point-cloud))
  (name :main :type symbol :read-only t)
  (points nil :type simple-array)
  (colors nil :type simple-array)
  (num-points 0 :type pos-int))


(defun -fill-arrays (data points colors)
  (loop for (pt c) in data
        for i from 0
        do (avec:3setv points i pt)
           (avec:3setv colors i c)))

(defun make (&key data fn name verbose)
  (declare (symbol name) (boolean verbose))
  "
  data is a list on the format ((pt0 c0) (pt1 c1) ...)
  "
  (if (and data fn) (error "use either :data or :fn"))
  (let* ((data* (if data data (dat:import-data fn)))
         (n (length data*))
         (points (avec:avec n :dim 3))
         (colors (avec:avec n :dim 3)))
    (-fill-arrays data* points colors)
    (when verbose (format t "point cloud size: ~a~%" n))
    (-make-point-cloud :name name :points points
                       :colors colors :num-points n)))


(defun get-num-points (ptc)
  (declare #.*opt-settings* (point-cloud ptc))
  (point-cloud-num-points ptc))


(defun vec->pigment (c &key (alpha 1d0))
  (declare #.*opt-settings* (vec:3vec c) (double-float alpha))
  (pigment:rgb (vec:3vec-x c) (vec:3vec-y c) (vec:3vec-z c) alpha))


(defun get-color (ptc i &key (alpha 1d0))
  (declare #.*opt-settings* (point-cloud ptc) (pos-int i) (double-float alpha))
  (vec->pigment (avec:3getv (point-cloud-colors ptc) i) :alpha alpha))


(declaim (inline make-point-getter))
(defun make-point-getter (ptc)
  (declare #.*opt-settings* (point-cloud ptc))
  (let* ((n (point-cloud-num-points ptc))
         (arr (point-cloud-points ptc))
         (points (loop with res = (make-array n :element-type 'vec:3vec :adjustable nil)
                       for i from 0 below (get-num-points ptc)
                       do (setf (aref res i) (avec:3getv arr i))
                       finally (return res))))
    (declare (3vec-simple points))
    (lambda (i) (declare (pos-int i)) (aref points i))))


(declaim (inline -center))
(defun -center (points v xy mx my mz)
  (avec:3with-vec (points v x y z)
    (setf x (+ (vec:3vec-x xy) (- x mx))
          y (+ (vec:3vec-y xy) (- y my))
          z (+ (vec:3vec-z xy) (- z mz)))))

(defun center! (ptc &key (xy vec:*3zero*))
  "
  center the points of ptc on xy. returns the previous center.
  "
  (with-struct (point-cloud- points num-points) ptc
    (declare (type (simple-array double-float) points)
             (pos-int num-points))
    (multiple-value-bind (minx maxx miny maxy minz maxz)
      (avec:3minmax points num-points)
      (let ((mx (* 0.5d0 (+ minx maxx)))
            (my (* 0.5d0 (+ miny maxy)))
            (mz (* 0.5d0 (+ minz maxz))))
        (declare (double-float mx my mz))
        (loop for v from 0 below num-points
              do (-center points v xy mx my mz))
        (vec:3vec mx my mz)))))

