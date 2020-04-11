
(in-package :pigment)


(declaim (inline -hex))
(defun -hex (d)
  (declare #.*opt-settings* (double-float d))
  (let ((res (format nil "~X" (min 255 (max 0 (floor (* d 256)))))))
    (if (< (length res) 2) (concatenate 'string "0" res) res)))

(defun to-hex (c)
  (declare #.*opt-settings* (rgba c))
  (destructuring-bind (r g b a) (to-list c)
    (values (apply #'concatenate 'string (list "#" (-hex r) (-hex g) (-hex b)))
            a)))


(defun cmyk (c m y k &optional (a 1d0))
  (declare #.*opt-settings* (double-float c m y k a))
  (let ((ik (- 1d0 k)))
    (make (* (- 1d0 c) ik) (* (- 1d0 m) ik) (* (- 1d0 y) ik) a)))


(defun hsv (h s v &optional (a 1d0))
  (declare #.*opt-settings* (double-float h s v a))
  (let* ((c (* v s))
         (x (* c (- 1d0 (abs (- (mod (* 6d0 h) 2d0) 1d0)))))
         (m (- v c)))
    (declare (double-float c x m))
    (destructuring-bind (r g b)
      (mapcar (lambda (v w) (+ v w))
              (case (floor (mod (* h 6d0) 6d0))
                    (0 (list c x 0d0))
                    (1 (list x c 0d0))
                    (2 (list 0d0 c x))
                    (3 (list 0d0 x c))
                    (4 (list x 0d0 c))
                    (5 (list c 0d0 x)))
              (list m m m))
      (declare (double-float r g b))
      (make r g b a))))


;(declaim (inline -mod))
(defun -mod (ca cb df &optional (p 1d0))
  (declare (double-float ca cb df p))
  ;(mod a b) is remainder of (floor a b)
  (multiple-value-bind (_ res)
    (floor (the double-float
                (+ p (* 0.16666666666666666d0
                        (/ (the double-float (- ca cb)) df)))))
    (declare (ignore _) (fixnum _) (double-float res))
    res))

(defun as-hsv (c)
  (declare #.*opt-settings* (rgba c))
  (-with (c r g b a)
    (let ((rgb (list r g b)))
      (destructuring-bind (imn mn) (math:argmin rgb)
        (declare (fixnum imn) (double-float mn))
        (destructuring-bind (imx mx) (math:argmax rgb)
          (declare (fixnum imx) (double-float mx))
          (let ((df (- mx mn)))
            (declare (double-float df))
            (list (cond ((= imn imx) 0d0)
                        ((= imx 0) (-mod g b df))
                        ((= imx 1) (-mod b r df 0.3333333333333333d0))
                        ((= imx 2) (-mod r g df 0.6666666666666666d0)))
                  (if (<= mx 0d0) 0d0 (/ df mx))
                  mx
                  a)))))))

(defun magenta (&key (sat 0.8d0) (val 0.85d0) (alpha 1d0))
  (hsv #.(/ 281d0 360d0) sat val alpha))

(defun cyan (&key (sat 0.8d0) (val 0.85d0) (alpha 1d0))
  (hsv #.(/ 196d0 360d0) sat val alpha))

(defun orange (&key (sat 0.8d0) (val 0.85d0) (alpha 1d0))
  (hsv #.(/ 38d0 360d0) sat val alpha))

(defun blood (&key (sat 0.8d0) (val 0.85d0) (alpha 1d0))
  (hsv #.(/ 362d0 360d0) sat val alpha))

