(in-package :vec)

(declaim (3vec *3one* *3zero*) (double-float *eps* PII))
(defparameter *3one* #.(3vec 1d0 1d0 1d0))
(defparameter *3zero* #.(3vec 0d0 0d0 0d0))
(defparameter *eps* 1d-14)


(declaim (inline 3from-vec))
(defun 3from-vec (v &key (z 0d0))
  "from 2d vec"
  (declare (vec v) (double-float z))
  (3vec (vec-x v) (vec-y v) z))


(declaim (inline 3to-vec))
(defun 3to-vec (v)
  "to 2d vec"
  (declare (3vec v))
  (vec (3vec-x v) (3vec-y v)))


(declaim (inline 3lto-vec))
(defun 3lto-vec (l)
  (declare (list l))
  (loop for v of-type 3vec in l collect (3to-vec v)))


(declaim (inline 3lfrom-vec))
(defun 3lfrom-vec (l &key (z 0d0))
  (declare (list l))
  (loop for v of-type vec in l collect (3from-vec v :z z)))


(declaim (inline 3to-list))
(defun 3to-list (v)
  (declare (3vec v))
  (list (3vec-x v) (3vec-y v) (3vec-z v)))


(declaim (inline 3vec*))
(defun 3vec* (xyz)
  "create vec from list"
  (declare #.*opt-settings* (list xyz))
  (apply #'3vec xyz))


(defun 3copy (xy)
  (declare (3vec xy))
  (3vec (3vec-x xy) (3vec-y xy) (3vec-z xy)))


; MATHS

(declaim (inline 3from))
(defun 3from (a b s)
  (declare #.*opt-settings* (double-float s) (3vec a b))
  (3vec (+ (3vec-x a) (* s (3vec-x b)))
        (+ (3vec-y a) (* s (3vec-y b)))
        (+ (3vec-z a) (* s (3vec-z b)))))

(declaim (inline 3from!))
(defun 3from! (a b s)
  (declare #.*opt-settings* (double-float s) (3vec a b))
  (3add! a (3smult b s)))


(declaim (inline 3neg))
(defun 3neg (a)
  (declare #.*opt-settings* (3vec a))
  (3smult a -1d0))


(declaim (inline 3vabs))
(defun 3vabs (a)
  (declare #.*opt-settings* (3vec a))
  (3vec (abs (3vec-x a)) (abs (3vec-y a)) (abs (3vec-z a))))


(declaim (inline 3dot))
(defun 3dot (a b)
  (declare #.*opt-settings* (3vec a b))
  (+ (* (3vec-x a) (3vec-x b))
     (* (3vec-y a) (3vec-y b))
     (* (3vec-z a) (3vec-z b))))


(declaim (inline 3ldot))
(defun 3ldot (aa bb)
  (declare #.*opt-settings* (list aa bb))
  (mapcar (lambda (a b) (declare (type 3vec a b)) (3dot a b)) aa bb))


(declaim (inline 3norm-project))
(defun 3norm-project (a b)
  (declare #.*opt-settings* (3vec a b))
  "projection of a onto b. assumes a, b are normalized to length 1"
  (3smult b (3dot a b)))


(declaim (inline 3norm-reflect))
(defun 3norm-reflect (d n)
  (declare #.*opt-settings* (3vec d n))
  "reflect d around plane normal n. assumes d and n are normalized."
  (3sub d (3smult n (* 2d0 (3dot d n)))))

(declaim (inline 3refract))
(defun 3refract (i n eta)
  (declare #.*opt-settings* (3vec i n) (double-float eta))
  (let* ((dni (3dot n i))
         (k (- 1d0 (* (* eta eta) (- 1d0 (* dni dni))))))
    (declare (double-float dni k))
    (if (<= k 0d0)
        ; total reflection
        (values *3zero* nil)
        ; refraction
        (values (3sub! (3smult i eta)
                       (3smult n (+ (* eta dni)
                                    (sqrt (the pos-double k)))))
                t))))


; TODO: this name is bad probably
(declaim (inline 3norm-reject))
(defun 3norm-reject (a b)
  (declare #.*opt-settings* (3vec a b))
  (3from a b (- (3dot a b))))


(declaim (inline 3len2))
(defun 3len2 (a)
  (declare #.*opt-settings* (3vec a))
  (+ (expt (3vec-x a) 2d0)
     (expt (3vec-y a) 2d0)
     (expt (3vec-z a) 2d0)))

(declaim (inline 3len))
(defun 3len (a)
  (declare #.*opt-settings* (3vec a))
  (sqrt (3len2 a)))


(declaim (inline 3mid))
(defun 3mid (a b)
  (declare #.*opt-settings* (3vec a b))
  (3smult (3add a b) 0.5d0))


(declaim (inline 3lmid))
(defun 3lmid (aa)
  (declare #.*opt-settings* (list aa))
  (let ((n 1))
    (declare (fixnum n))
    (3smult
      (reduce (lambda (a b) (declare (type 3vec a b)) (incf n) (3add a b)) aa)
      (/ (coerce n 'double-float)))))


(declaim (inline 3dst2))
(defun 3dst2 (a b)
  (declare #.*opt-settings* (3vec a b))
  (+ (expt (- (3vec-x a) (3vec-x b)) 2d0)
     (expt (- (3vec-y a) (3vec-y b)) 2d0)
     (expt (- (3vec-z a) (3vec-z b)) 2d0)))

(declaim (inline 3dst))
(defun 3dst (a b)
  (declare #.*opt-settings* (3vec a b))
  (sqrt (3dst2 a b)))


(declaim (inline 3dst*))
(defun 3dst* (aa)
  (declare #.*opt-settings* (list aa))
  (apply #'3dst aa))


(declaim (inline 3ldst))
(defun 3ldst (a b)
  (declare #.*opt-settings* (list a b))
  (mapcar #'3dst a b))


(declaim (inline 3ldst*))
(defun 3ldst* (aa b)
  (declare #.*opt-settings* (list aa) (3vec b))
  (loop for a of-type 3vec in aa collect (3dst a b)))


(declaim (inline 3norm))
(defun 3norm (a &key (s 1d0) (default *3zero*))
  (declare #.*opt-settings* (3vec a) (double-float s))
  (let ((l (3len a)))
    (if (> l 0d0) (3smult a (/ s l)) default)))

(declaim (inline 3norm!))
(defun 3norm! (a &key (s 1d0) (default *3zero*))
  (declare #.*opt-settings* (3vec a) (double-float s))
  (let ((l (3len a)))
    (if (> l 0d0) (3smult! a (/ s l))
        (3set! a default))))


(declaim (inline 3maxnrm))
(defun 3maxnrm (a)
  (declare #.*opt-settings* (3vec a))
  (max (abs (3vec-x a)) (abs (3vec-y a)) (abs (3vec-z a))))


(declaim (inline 3nsub))
(defun 3nsub (a b &key (s 1d0) (default *3zero*))
  (declare (3vec a b))
  (3norm (3sub a b) :s s :default default))


(declaim (inline 3lsum))
(defun 3lsum (l)
  (declare #.*opt-settings* (list l))
  (loop with res = (3zero)
        for a in l
        do (3add! res a)
        finally (return res)))


(declaim (inline 3cross))
(defun 3cross (a b)
  (declare #.*opt-settings* (3vec a b))
  (3with-xy (a a1 a2 a3)
    (3with-xy (b b1 b2 b3)
      (3vec (- (* a2 b3) (* a3 b2))
            (- (* a3 b1) (* a1 b3))
            (- (* a1 b2) (* a2 b1))))))


(declaim (inline 3cross!))
(defun 3cross! (a b)
  (declare #.*opt-settings* (3vec a b))
  (3with-xy (a a1 a2 a3)
    (3with-xy (b b1 b2 b3)
      (setf (3vec-x a) (- (* a2 b3) (* a3 b2))
            (3vec-y a) (- (* a3 b1) (* a1 b3))
            (3vec-z a) (- (* a1 b2) (* a2 b1)))))
  a)


(declaim (inline 3rot))
(defun 3rot (pt plane-vec a &key xy)
  (declare #.*opt-settings* (3vec pt plane-vec) (double-float a))
  (let* ((pt* (if xy (3sub pt xy) pt))
         (cosa (cos a))
         (res (3from (3from (3smult pt* cosa) (3cross plane-vec pt*) (sin a))
                     plane-vec (* (3dot plane-vec pt*) (- 1d0 cosa)))))
    (declare (3vec pt* res) (double-float cosa))
    (if xy (3add res xy) res)))

(declaim (inline 3lrot*))
(defun 3lrot* (l k a &key xy)
  (declare #.*opt-settings* (list l) (3vec k) (double-float a))
  (loop for v of-type 3vec in l
        ;TODO: optimize this call to avoid cos/sin calcs
        collect (3rot v k a :xy xy)))


(declaim (inline 3on-line))
(defun 3on-line (p a b)
  (declare #.*opt-settings* (double-float p) (3vec a b))
  (3vec (+ (3vec-x a) (* p (- (3vec-x b) (3vec-x a))))
        (+ (3vec-y a) (* p (- (3vec-y b) (3vec-y a))))
        (+ (3vec-z a) (* p (- (3vec-z b) (3vec-z a))))))


(declaim (inline 3lon-line))
(defun 3lon-line (p ab)
  (declare #.*opt-settings* (double-float p) (list ab))
    (apply #'3on-line p ab))


(declaim (inline 3lon-line*))
(defun 3lon-line* (pp ab)
  (declare #.*opt-settings* (sequence pp) (list ab))
  (destructuring-bind (a b) ab
    (declare (3vec a b))
    (if (equal (type-of pp) 'cons)
        (loop for p of-type double-float
                in pp collect (3on-line p a b))
        (loop for p of-type double-float
                across (the (simple-array double-float) pp)
              collect (3on-line p a b)))))


(declaim (inline -plane-test))
(defun -plane-test (p l0 n s)
  (declare #.*opt-settings* (3vec p l0 n) (double-float s))
  ; (/ (3dot (3sub p l0) n) s)
  (/ (+ (* (- (3vec-x p) (3vec-x l0)) (3vec-x n))
        (* (- (3vec-y p) (3vec-y l0)) (3vec-y n))
        (* (- (3vec-z p) (3vec-z l0)) (3vec-z n)))
     s))

(defun 3planex (n p line)
  (declare #.*opt-settings* (3vec n p) (list line))
  "intersection of plane (n:normal, p:point) and line"
  (destructuring-bind (l0 l1) line
    (declare (3vec l0 l1))
    (let* ((ln (3sub l1 l0))
           (ldotn (3dot ln n)))
      (declare (3vec ln) (double-float ldotn))
      ; avoid div0.
      (when (< (abs ldotn) 1d-14)
            (return-from 3planex (values nil 0d0 (3zero))))
      ; else
      (let ((d (-plane-test p l0 n ldotn)))
        (declare (double-float d))
        (values t d (3from l0 ln d))))))

;https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
(declaim (inline -3polyx))
(defun -3polyx (v0 e1 e2 org l)
  (declare #.*opt-settings* (3vec org l v0 e1 e2))
  (let* ((h (3cross l e2))
         (a (3dot e1 h)))
    (declare (3vec h) (double-float a))

  ; parallel
  (when (< (abs a) *eps*)
        (return-from -3polyx (values nil 0d0 *3zero*)))

  (let* ((f (/ a))
         (s (3sub org v0))
         (u (* f (3dot s h))))
    (declare (3vec s) (double-float f u))

    (when (or (> u 1d0) (< u 0d0))
          (return-from -3polyx (values nil 0d0 *3zero*)))

    (let* ((q (3cross! s e1))
           (v (* f (3dot l q))))
      (declare (3vec q) (double-float v))
      (when (or (< v 0d0) (> (+ u v) 1d0))
            (return-from -3polyx (values nil 0d0 *3zero*)))

      (let ((tt (* f (3dot e2 q))))
        (declare (double-float tt))
        (if (> 1d0 tt *eps*)
            ; intersection on line
            (values t tt (3from org l tt))
            ; intersection (not on line)
            (values nil tt (3from org l tt))))))))

(defun 3polyx (verts line )
  (declare #.*opt-settings* (list verts line))
  (destructuring-bind (v0 v1 v2) verts
    (declare (3vec v0 v1 v2))
    (-3polyx v0 (3sub v1 v0) (3sub v2 v0)
             (first line) (apply #'3isub line))))


; https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
(declaim (inline -3spherex))
(defun -3spherex (r oc l llen)
  (declare #.*opt-settings* (3vec oc l) (double-float r llen))
  (let* ((ldotoc (3dot l oc))
         (s (- (expt ldotoc 2d0) (- (3len2 oc) (* r r)))))
      (declare (double-float ldotoc s))

      (when (< s 0d0) (return-from -3spherex (values nil nil)))
      (when (> s 0d0) (return-from -3spherex
                        (values t (list (/ (- s ldotoc) llen)
                                        (/ (- (- ldotoc) s) llen)))))
      (values t (list (/ (- ldotoc) llen)))))

(declaim (inline 3spherex))
(defun 3spherex (c r line)
  (declare #.*opt-settings* (3vec c) (double-float r) (list line))
  "intersection of sphere (c, r) and line (o e)"
  (destructuring-bind (o e) line
    (declare (3vec o e))
    (let* ((ll (3sub e o))
           (llen (3len ll)))
      (declare (3vec ll) (double-float llen))
      (-3spherex r (3sub o c) (3sdiv ll llen) llen))))

