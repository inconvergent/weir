
(in-package :vec)

(declaim (type double-float PII) (vec *one* *zero*))
(defconstant PII  #.(* PI 2d0))
(defparameter *one* #.(vec 1d0 1d0))
(defparameter *zero* #.(vec 0d0 0d0))


(defmacro nxn ((p &key nx sx ny sy mid bx by) &body body)
  (alexandria:with-gensyms (x y nx* ny** ny* sx* sy** sy*
                            bx* by** by* mid* mid** mx* my*)
    `(let* ((,nx* ,nx) (,sx* ,sx)
            (,ny** ,ny) (,sy** ,sy)
            (,bx* ,bx) (,by** ,by)
            (,mid** ,mid)
            (,ny* (if ,ny** ,ny** ,nx*))
            (,sy* (if ,sy** ,sy** ,sx*))
            (,by* (if ,by** ,by** ,bx*))
            (,mid* (if ,mid** ,mid** (zero)))
            (,mx* (vec-x ,mid*))
            (,my* (vec-y ,mid*)))
      (declare (fixnum ,nx* ,ny*) (double-float ,sx* ,sy*) (vec ,mid*))
      (loop for ,x of-type double-float
              in (math:linspace ,nx* (- ,mx* ,sx* (- ,bx*))
                                     (+ ,mx* ,sx* (- ,bx*)))
            do (loop for ,y of-type double-float
                       in (math:linspace ,ny* (- ,my* ,sy* (- ,by*))
                                              (+ ,my* ,sy* (- ,by*)))
                     do (let ((,p (vec ,x ,y)))
                          (declare (vec ,p))
                          (progn ,@body)))))))


(declaim (inline hline))
(defun hline (s &key (xy (zero)))
  (declare #.*opt-settings* (double-float s) (vec xy))
  "horizontal line top to bottom, size 2*s around xy "
  (vec:ladd!* (list (vec:vec (- s) 0d0) (vec:vec s 0d0)) xy))

(declaim (inline vline))
(defun vline (s &key (xy (zero)))
  (declare #.*opt-settings* (double-float s) (vec xy))
  "vertical line top to bottom, size 2*s around xy"
  (vec:ladd!* (list (vec:vec 0d0 (- s)) (vec:vec 0d0 s)) xy))

(declaim (inline rline))
(defun rline (s angle &key (xy (zero)) &aux (converse (+ angle PI)))
  (declare #.*opt-settings* (double-float s angle converse) (vec xy))
  "line rotated at angle, size 2*s around xy"
  (vec:ladd!* (vec:lsmult!* (list (vec:vec (cos angle) (sin angle))
                                  (vec:vec (cos converse) (sin converse))) s)
              xy))


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
  (add! a (smult b s)))


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
  (loop with res = (zero)
        for a in l do (add! res a)
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
  "lerp between vecs, a, b"
  (vec (+ (vec-x a) (* p (- (vec-x b) (vec-x a))))
       (+ (vec-y a) (* p (- (vec-y b) (vec-y a))))))


(declaim (inline lon-line))
(defun lon-line (p ab)
  (declare #.*opt-settings* (double-float p) (list ab))
  "lerp on line, ab"
  (apply #'on-line p ab))


(declaim (inline lon-line*))
(defun lon-line* (pp ab)
  (declare #.*opt-settings* (sequence pp) (list ab))
  "lerp multiple on line, ab"
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
  (loop with pin of-type double-float = (/ PII n)
        for i of-type fixnum from 0 below n
        collect (from xy (cos-sin (+ rot (* i pin))) rad)))

