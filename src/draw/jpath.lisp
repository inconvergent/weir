
(in-package :jpath)


(defvar *pi3* (/ PI 3d0))
(defvar *pi2* (/ PI 2d0))
(defvar *pi23* (* 2d0 (/ PI 3d0)))
(defvar *limits* (list (/ PI 2.99d0) (/ PI 3.99d0) (/ PI 7.99d0)))

(defun ht () (make-hash-table :test #'equal))
(defun ori (in out) (> (vec:cross in out) 0d0))
(defmacro o++ (p v w) `(vec:add! (vec:add p ,v) ,w))
(defmacro o-- (p v w) `(vec:sub! (vec:sub p ,v) ,w))
(defmacro o+- (p v w) `(vec:sub! (vec:add p ,v) ,w))
(defmacro o-+ (p v w) `(vec:add! (vec:sub p ,v) ,w))

(defstruct (joint (:constructor -make-joint))
  (p (vec:zero) :type vec:vec :read-only t)
  (w 0d0 :type double-float :read-only t)
  (orientation t :type boolean :read-only t)
  (alpha 0d0 :type double-float :read-only t)
  (in (vec:zero) :type vec:vec :read-only t)
  (out (vec:zero) :type vec:vec :read-only t)
  (mode :joint :type symbol :read-only t)
  (grid nil :type vector :read-only t)
  (i 0 :type fixnum :read-only t))



(defun -make-joint-grid (p i o)
  "
  8 offset points around p
  "
  (let* ((z (vec:zero))
         (res (to-adjustable-vector
      (list (o+- p o i) (o+- p z i) (o-- p o i) (o-+ p o z)
            (o-+ p o i) (o++ p z i) (o++ p o i) (o++ p o z)))))
    (vextend p res)
    res))


(defun path->joints (path* w* &key closed &aux (w (* 0.5d0 w*)))
  (declare (list path*) (double-float w w*) (boolean closed))
  "
  joints contain information about how to offset around points in path.
  "
  (let* ((path (ensure-vector path*))
         (n (length path)))
    (labels
      ((make-joint (i a p b)
         (let* ((in (vec:norm (vec:sub p a)))
                (out (vec:norm (vec:sub b p)))
                (alpha (- PI (acos (vec:dot in out))))
                (s (/ w (sin alpha))))
           (vec:smult! in s)
           (vec:smult! out s)
           (-make-joint :p p :w w :i i :alpha alpha :in in :out out
                        :orientation (ori in out) :grid (-make-joint-grid p in out))))
       (make-start (p b)
         (let* ((out (vec:norm! (vec:sub b p) :s w))
                (in (vec:rot out *pi2*)))
           (-make-joint :p p :w w :mode :start :alpha *pi2*
                        :in in :out out :grid (-make-joint-grid p in out))))
       (make-end (i a p)
         (let* ((in (vec:norm! (vec:sub p a) :s w))
                (out (vec:rot in (- *pi2*))))
           (-make-joint :p p :w w :i i :mode :end :alpha (- *pi2*)
                        :in in :out out :grid (-make-joint-grid p in out))))
       (ci (i) (aref path (mod i n)))
       (closed-path->joints ()
         (loop for i from 0 below n
               collect (make-joint i (ci (1- i)) (ci i) (ci (1+ i)))))
       (open-path->joints ()
         (loop with init = (make-end (1- n) (ci (- n 2)) (ci (1- n)))
               with res = (list init)
               for i from (- n 2) downto 1
               do (push (make-joint i (ci (1- i)) (ci i) (ci (1+ i))) res)
               finally (return (cons (make-start (ci 0) (ci 1)) res)))))
      (when (or (and (not closed) (< n 2)) (and closed (< n 3)))
            (error "jpath must have at least 2 (open) or 3 (closed) elements."))
      (if closed (closed-path->joints) (open-path->joints)))))

(defun path->diagonals (path w &key closed (limits *limits*))
  (declare (list path) (double-float w) (boolean closed))
  "
  return (orientation line) for every point in path. lerp-ing along lines will
  return controll points. lerp direction should be flipped when orientation is
  nil.  sharp or chamfered points correspond to two lines
  "
  (let ((joints (to-vector (path->joints path w :closed closed)))
        (res (list))
        (la (first limits))
        (lb (second limits))
        (lc (third limits)))
    (labels
       ((gx (a p) (aref (joint-grid (aref joints a)) p))
        (start (p) (list (gx p 1) (gx p 5)))
        (end (p) (list (gx p 7) (gx p 3)))
        (joint (p) (list (gx p 4) (gx p 0)))
        (soft-1 (p) (list (gx p 3) (gx p 0)))
        (soft-2 (p) (list (gx p 5) (gx p 0)))
        (chamfer-1 (p) (list (gx p 2) (gx p 0)))
        (chamfer-2 (p) (list (gx p 6) (gx p 0)))
        ;(sharp-1 (p) (list (gx p 2) (gx p 0)))
        ;(sharp-2 (p) (list (gx p 6) (gx p 0)))
        (sharp-3 (p) (list (gx p 2) (gx p 6)))
        (sharp-4 (p) (list (gx p 6) (gx p 2)))
        (do-joint (i)
          (let* ((j (aref joints i))
                 (alpha (joint-alpha j))
                 (ori (joint-orientation j)))
            (cond ((<= alpha lc) (push (list ori (sharp-3 i)) res)
                                 (push (list ori (sharp-4 i)) res))
                  ((<= alpha lb) (push (list ori (chamfer-1 i)) res)
                                 (push (list ori (chamfer-2 i)) res))
                  ((<= alpha la) (push (list ori (soft-1 i)) res)
                                 (push (list ori (soft-2 i)) res))
                  (t (push (list ori (joint i)) res))))))

       (loop for i from 0 below (length joints)
             do (case (joint-mode (aref joints i))
                      (:end (push (list t (end i)) res))
                      (:start (push (list t (start i)) res))
                      (:joint (do-joint i))
                      (t (error "unknown joint mode"))))
       (reverse res))))


(defun jpath (path w &key (rep 3) closed (limits *limits*))
  (declare (list path) (double-float w) (fixnum rep) (boolean closed))
  (let* ((diagonals (to-vector (path->diagonals path w :closed closed :limits limits)))
         (n (length diagonals))
         (res (make-adjustable-vector))
         (ss (math:linspace rep 0d0 1d0)))

    (labels
      ((flip? (ori s) (if ori s (- 1d0 s)))
       (open-ind (i k i-) (if (= (math:mod2 k) 0) i i-))
       (closed-path ()
         (loop for s in ss
               do (vextend (loop for (ori line) across diagonals
                                 collect (vec:lon-line (flip? ori s) line))
                           res)))
       (open-path ()
         (loop for s in ss and k of-type fixnum from 0
               do (loop for i of-type fixnum from 0 below n
                        and i- of-type fixnum downfrom (1- n)
                        do (destructuring-bind (ori line)
                             (aref diagonals (open-ind i k i- ))
                             (vextend (vec:lon-line (flip? ori s) line) res))))))
      (if closed (closed-path) (open-path))
      (to-list res))))

