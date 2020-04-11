
(in-package :lin-path)


(defstruct path
  (n nil :type fixnum :read-only t)
  (closed nil :type boolean)
  (pts nil :type (simple-array double-float) :read-only t)
  (tot nil :type double-float :read-only t)
  (lerpfx nil :type function :read-only t)
  (weights nil :type (simple-array double-float) :read-only t))


(defun -set-weights (pts weights n &key dstfx)
  (declare #.*opt-settings*
           (type (simple-array double-float) pts weights)
           (fixnum n)
           (function dstfx))
  (loop with tot of-type double-float =
          (loop for i of-type fixnum from 0 below (1- n)
                sum (funcall dstfx pts pts i (1+ i)) into tot of-type double-float
                do (setf (aref weights (1+ i)) tot)
                finally (return tot))
        for i of-type fixnum from 1 below n
        do (setf (aref weights i) (/ (aref weights i) tot))
        finally (return tot)))


(defun -find-seg-ind (weights f n)
  (declare #.*opt-settings*
           (type (simple-array double-float) weights)
           (fixnum n) (double-float f))
  (loop with l of-type fixnum = 0
        with r of-type fixnum = (- n 1)
        with mid of-type fixnum = 0
        until (<= (aref weights mid) f (aref weights (1+ mid)))
        do (setf mid (floor (+ l r) 2))
           (cond ((> f (aref weights mid)) (setf l mid))
                 ((<= f (aref weights mid)) (setf r (1+ mid))))
        finally (return (the fixnum (1+ mid)))))


(defun -lerp (pts i s &aux (bi (* 2 i)) (ai (- bi 2)))
  (declare #.*opt-settings*
           (type (simple-array double-float) pts)
           (fixnum i bi ai) (double-float s))
  (vec:vec (+ (aref pts ai) (* (- (aref pts bi)
                                  (aref pts ai)) s))
           (+ (aref pts (1+ ai)) (* (- (aref pts (1+ bi))
                                       (aref pts (1+ ai))) s))))

(defun -lerp3 (pts i s &aux (bi (* 3 i)) (ai (- bi 3)))
  (declare #.*opt-settings*
           (type (simple-array double-float) pts)
           (fixnum i bi ai) (double-float s))
  (vec:3vec (+ (aref pts ai)
               (* (- (aref pts bi)
                     (aref pts ai)) s))
            (+ (aref pts (1+ ai))
               (* (- (aref pts (1+ bi))
                     (aref pts (1+ ai))) s))
            (+ (aref pts (+ 2 ai))
               (* (- (aref pts (+ bi 2))
                     (aref pts (+ ai 2))) s))))


(defun -scale-weight (a b s)
  (declare #.*opt-settings* (double-float a b s))
  (/ (- b a) s))

(declaim (inline -mod))
(defun -dmod (x)
  (declare (double-float x))
  (when (<= 0d0 x 1d0) (return-from -dmod x))
  (multiple-value-bind (_ res) (floor x)
    (declare (ignore _) (double-float res))
    res))

(defun -pos (pts weights n f &key (modf (-dmod f)) lerpfx)
  (declare #.*opt-settings*
           (type (simple-array double-float) pts weights)
           (fixnum n) (double-float f modf) (function lerpfx))
  (let ((i (-find-seg-ind weights modf n)))
    (declare (fixnum i))
    (funcall lerpfx pts i
             (-scale-weight (aref weights (1- i))
                            modf
                            (- (aref weights i) (aref weights (1- i)))))))


(defun pos (pth f)
  (declare (path pth) (double-float f))
  (with-struct (path- weights pts lerpfx n) pth
    (-pos pts weights n f :lerpfx lerpfx)))


(defun pos* (pth ff)
  (declare (path pth) (list ff))
  (with-struct (path- weights pts lerpfx n) pth
    (mapcar (lambda (f) (declare (double-float f))
              (-pos pts weights n f :lerpfx lerpfx))
            ff)))


(defun rndpos (pth n)
  (rnd:with-rndspace (n 0d0 1d0 p :collect t)
    (pos pth p)))


(defun make (pts &key (dim 2) closed)
  (declare (list pts) (fixnum dim) (boolean closed))
  (when (not (< 1 dim 4)) (error "dim must be 2 or 3"))
  (let* ((n (if closed (+ (length pts) 1) (length pts)))
         (p (avec:avec n :dim dim))
         (l (avec:avec n :dim 1))
         (setfx (if (= dim 2) #'avec:setv #'avec:3setv)))
    (declare (fixnum n) (type (simple-array double-float) p l)
             (function setfx))
    (loop for pt in pts and i of-type fixnum from 0
          do (funcall setfx p i pt))
    (when closed (funcall setfx p (1- n) (first pts)))
    (make-path :n n :pts p
               :weights l
               :closed closed
               :lerpfx (if (= dim 2) #'-lerp #'-lerp3)
               :tot (-set-weights p l n
                      :dstfx (if (= dim 2) #'avec:dst #'avec:3dst)))))

