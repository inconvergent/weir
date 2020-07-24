
(in-package :vec)

(declaim (type double-float PII) (vec:vec *one* *zero*))
(defconstant PII  #.(* PI 2d0))
(defparameter *one* #.(vec 1d0 1d0))
(defparameter *zero* #.(vec 0d0 0d0))


(declaim (inline to-list))
(defun to-list (v)
  (declare #.*opt-settings* (vec v))
  (list (vec-x v) (vec-y v)))


(declaim (inline cross))
(defun cross (a b)
  (declare #.*opt-settings* (vec a b))
  (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b))))


(declaim (inline flip))
(defun flip (v)
  (declare #.*opt-settings* (vec v))
  (vec (vec-y v) (vec-x v)))


(declaim (inline perp))
(defun perp (v)
  (declare #.*opt-settings* (vec v))
  (vec (vec-y v) (- (vec-x v))))


(declaim (inline vround))
(defun vround (v)
  (declare #.*opt-settings* (vec v))
  (list (round (vec-x v)) (round (vec-y v))))


(declaim (inline vec*))
(defun vec* (xy)
  "create vec from list"
  (declare #.*opt-settings* (list xy))
  (apply #'vec xy))


(defun copy (xy)
  (declare #.*opt-settings* (vec xy))
  (vec (vec-x xy) (vec-y xy)))


; MATHS

(declaim (inline cos-sin))
(defun cos-sin (a)
  (declare #.*opt-settings* (double-float a))
  (vec (cos a) (sin a)))


(declaim (inline cos-negsin))
(defun cos-negsin (a)
  (declare #.*opt-settings* (double-float a))
  (vec (cos a) (- (sin a))))


(declaim (inline sin-cos))
(defun sin-cos (a)
  (declare #.*opt-settings* (double-float a))
  (vec (sin a) (cos a)))

(declaim (inline from))
(defun from (a b s)
  (declare #.*opt-settings* (double-float s) (vec a b))
  (vec (+ (vec-x a) (* s (vec-x b)))
       (+ (vec-y a) (* s (vec-y b)))))


(declaim (inline from!))
(defun from! (a b s)
  (declare #.*opt-settings* (double-float s) (vec a b))
  (vec:add! a (vec:smult b s)))


(declaim (inline neg))
(defun neg (a)
  (declare #.*opt-settings* (vec a))
  (smult a -1d0))


(declaim (inline vabs))
(defun vabs (a)
  (declare #.*opt-settings* (vec a))
  (vec (abs (vec-x a)) (abs (vec-y a))))


(declaim (inline dot))
(defun dot (a b)
  (declare #.*opt-settings* (vec a b))
  (+ (* (vec-x a) (vec-x b)) (* (vec-y a) (vec-y b))))


(declaim (inline ldot))
(defun ldot (aa bb)
  (declare #.*opt-settings* (list aa bb))
  (mapcar (lambda (a b) (declare (type vec a b)) (dot a b)) aa bb))


(declaim (inline norm-project))
(defun norm-project (a b)
  (declare #.*opt-settings* (vec a b))
  "projection of a onto b. assumes a, b are normalized to length 1"
  (smult b (dot a b)))


(declaim (inline norm-reflect))
(defun norm-reflect (d n)
  (declare #.*opt-settings* (vec d n))
  "reflect d around plane normal n. assumes d and n are normalized."
  (sub d (smult n (* 2d0 (dot d n)))))


(declaim (inline refract))
(defun refract (i n eta)
  (declare #.*opt-settings* (vec i n) (double-float eta))
  (let* ((dni (dot n i))
         (k (- 1d0 (* (* eta eta) (- 1d0 (* dni dni))))))
    (declare (double-float dni k))
    (if (<= k 0d0)
        ; total reflection
        (values *zero* nil)
        ; refraction
        (values (sub! (smult i eta)
                      (smult n (+ (* eta dni)
                                  (sqrt (the pos-double k)))))
                t))))


(declaim (inline norm-reject))
(defun norm-reject (a b)
  (declare #.*opt-settings* (vec a b))
  (sub a (smult b (dot a b))))


(declaim (inline len2))
(defun len2 (a)
  (declare #.*opt-settings* (vec a))
  (+ (expt (vec-x a) 2d0) (expt (vec-y a) 2d0)))

(declaim (inline len))
(defun len (a)
  (declare #.*opt-settings* (vec a))
  (sqrt (len2 a)))


(declaim (inline mid))
(defun mid (a b)
  (declare #.*opt-settings* (vec a b))
  (smult (add a b) 0.5d0))

(declaim (inline lmid))
(defun lmid (aa)
  (declare #.*opt-settings* (list aa))
  (let ((n 1))
    (declare (fixnum n))
    (smult
      (reduce (lambda (a b) (declare (type vec a b)) (incf n) (add a b)) aa)
      (/ (coerce n 'double-float)))))


(declaim (inline dst2))
(defun dst2 (a b)
  (declare #.*opt-settings* (vec a b))
  (+ (expt (- (vec-x a) (vec-x b)) 2d0) (expt (- (vec-y a) (vec-y b)) 2d0)))


(declaim (inline dst))
(defun dst (a b)
  (declare #.*opt-settings* (vec a b))
  (sqrt (dst2 a b)))

(declaim (inline dst*))
(defun dst* (aa)
  (declare #.*opt-settings* (list aa))
  (apply #'dst aa))

(declaim (inline ldst))
(defun ldst (a b)
  (declare #.*opt-settings* (list a b))
  (mapcar #'dst a b))

(declaim (inline ldst*))
(defun ldst* (aa b)
  (declare #.*opt-settings* (list aa) (vec b))
  (loop for a of-type vec in aa collect (dst a b)))


(declaim (inline norm))
(defun norm (a &key (s 1d0) (default *zero*))
  (declare #.*opt-settings* (vec a) (double-float s))
  (let ((l (len a)))
    (declare (double-float l))
    (if (> l 0d0) (smult a (/ s l)) default)))

(declaim (inline norm!))
(defun norm! (a &key (s 1d0) (default *zero*))
  (declare #.*opt-settings* (vec a) (double-float s))
  (let ((l (len a)))
    (if (> l 0d0) (smult! a (/ s l))
        (set! a default))))

(declaim (inline maxnrm))
(defun maxnrm (a)
  (declare #.*opt-settings* (vec a))
  (max (abs (vec-x a)) (abs (vec-y a))))


(declaim (inline angle))
(defun angle (v)
  (declare #.*opt-settings* (vec v))
  (with-xy ((norm v) x y) (atan y x)))


(declaim (inline nsub))
(defun nsub (a b &key (s 1d0) (default *zero*))
  (declare (vec a b))
  (norm (sub a b) :s s :default default))


(declaim (inline lsum))
(defun lsum (l)
  (declare #.*opt-settings* (list l))
  (loop with res = (vec:zero)
        for a in l
        do (vec:add! res a)
        finally (return res)))


(declaim (inline rot))
(defun rot (v a &key (xy *zero*))
  (declare #.*opt-settings* (vec v) (double-float a))
  (let ((cosa (cos a))
        (sina (sin a)))
    (declare (double-float cosa sina))
    (with-xy ((sub v xy) x y)
      (add xy (vec (- (* x cosa) (* y sina))
                   (+ (* x sina) (* y cosa)))))))


(declaim (inline lrot))
(defun lrot (pts a &key (xy *zero*))
  (declare #.*opt-settings* (list pts) (double-float a))
  (mapcar (lambda (p) (declare (vec p)) (rot p a :xy xy)) pts))


; SHAPES

(declaim (inline on-circ))
(defun on-circ (p rad &key (xy *zero*))
  (declare #.*opt-settings* (double-float p rad) (vec xy))
  (from xy (cos-sin (* p PII)) rad))


(declaim (inline on-line))
(defun on-line (p a b)
  (declare #.*opt-settings* (double-float p) (vec a b))
  (vec (+ (vec-x a) (* p (- (vec-x b) (vec-x a))))
       (+ (vec-y a) (* p (- (vec-y b) (vec-y a))))))


(declaim (inline lon-line))
(defun lon-line (p ab)
  (declare #.*opt-settings* (double-float p) (list ab))
  (apply #'on-line p ab))


(declaim (inline lon-line*))
(defun lon-line* (pp ab)
  (declare #.*opt-settings* (sequence pp) (list ab))
  (if (equal (type-of pp) 'cons)
      (loop for p of-type double-float in pp collect (lon-line p ab))
      (loop for p of-type double-float across pp collect (lon-line p ab))))


(declaim (inline rect))
(defun rect (w h &key (xy *zero*))
  (declare #.*opt-settings* (double-float w h) (vec xy))
  (list (add xy (vec w (- h))) (add xy (vec w h))
        (add xy (vec (- w) h)) (sub xy (vec w h))))


(declaim (inline square))
(defun square (bs &key (xy *zero*))
  (declare #.*opt-settings* (double-float bs) (vec xy))
  (rect bs bs :xy xy))


(declaim (inline polygon))
(defun polygon (n rad &key (xy *zero*) (rot 0d0))
  (declare #.*opt-settings* (fixnum n) (double-float rad rot) (vec xy))
  (loop for i of-type fixnum from 0 below n
        collect (from xy (cos-sin
                           (+ rot (* (/ (coerce i 'double-float) n) PII))) rad)))


(declaim (inline segdst))
(defun segdst (line v)
  "
  find distance between line and v.
  returns values (distance s) where is is the interpolation value that will
  yield the closest point on line.
  "
  (declare #.*opt-settings* (list line) (vec v))
  (destructuring-bind (va vb) line
    (declare (vec va vb))
    (let ((l2 (dst2 va vb)))
      (declare (double-float l2))
      (if (<= l2 0d0)
        ; line is a point
        (values (dst va v) 0d0)
        ; else
        (let ((tt (/ (+ (* (- (vec-x v) (vec-x va)) (- (vec-x vb) (vec-x va)))
                        (* (- (vec-y v) (vec-y va)) (- (vec-y vb) (vec-y va))))
                     l2)))
          (if (> tt 1d0) (setf tt 1d0))
          (if (< tt 0d0) (setf tt 0d0))
          (values (dst v (on-line tt va vb)) tt))))))


; TODO: this is slow?
(declaim (inline segx))
(defun segx (aa bb)
  (declare #.*opt-settings* (list aa bb))
  (destructuring-bind (a1 a2) aa
    (declare (vec a1 a2))
    (destructuring-bind (b1 b2) bb
      (declare (vec b1 b2))
      (let* ((sa (sub a2 a1))
             (sb (sub b2 b1))
             (u (cross sa sb)))
        (declare (vec sa sb) (double-float u))
        (if (<= (abs u) 0d0)
          ; return nil if the lines are parallel (nil)
          ; this is just a div0 guard. it's not a good way to test.
          (values nil 0d0 0d0)
          ; otherwise check if they intersect
          (let ((p (/ (cross sa #1=(sub a1 b1)) u))
                (q (/ (cross sb #1#) u)))
            (declare (double-float p q))
            ; t if intersection, nil otherwise
            (values (and (> p 0d0) (< p 1d0) (> q 0d0) (< q 1d0))
                    q p)))))))


(declaim (inline ptinside))
(defun ptinside (convex v)
  (declare #.*opt-settings* (list convex) (vec v))
  (loop for a of-type vec in (math:close-path convex)
        and b of-type vec in (cdr (math:close-path convex))
        always (>= (cross (sub b a) (sub v b)) 0d0)))

