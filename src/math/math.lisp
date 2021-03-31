
(in-package :math)

(deftype pos-int (&optional (bits 31)) `(unsigned-byte ,bits))


(declaim (inline clamp))
(defun clamp (v mi ma)
  (declare #.*opt-settings* (double-float v mi ma))
  (max (min v ma) mi))


(declaim (inline last*))
(defun last* (l)
  (declare #.*opt-settings* (list l))
  "last element of l"
  (first (last l)))

(declaim (inline close-path))
(defun close-path (p)
  (declare #.*opt-settings* (list p))
  "append first element of p to end of p"
  (append p (subseq p 0 1)))

(declaim (inline close-path*))
(defun close-path* (p)
  (declare #.*opt-settings* (list p))
  "append last element of p to front of p"
  (cons (last* p) p))


; RANGES


(defmacro nrep (n &body body)
  "returns list with body repeated n times"
  (alexandria:with-gensyms (nname)
    `(loop with ,nname of-type pos-int = ,n
           repeat ,nname collect (progn ,@body))))


(defun range (a &optional (b nil))
  (declare #.*opt-settings* (fixnum a))
  "fixnums from 0 to a, or a to b."
  (if (not b) (loop for x of-type fixnum from 0 below a collect x)
              (loop for x of-type fixnum from a below (the fixnum b)
                    collect x)))


(defun lpos (ll &key (fx #'first))
  (declare (list ll) (function fx))
  "
  apply fx to every element in ll.
  eg get first element of lists in list of lists
  "
  (mapcar fx ll))


(defun lget (l ii)
  "get indices ii from l"
  (declare #.*opt-settings* (list l ii))
  (loop with arr of-type vector = (to-vector l)
        for i of-type fixnum in ii collect (aref arr i)))


; TODO pretty sure there is a better way to do this
(declaim (inline ll-transpose))
(defun ll-transpose (l)
  (declare #.*opt-settings* (list l))
  "transpose list of lists"
  (labels ((-reduce (acc v) (loop for a in acc and b in v collect (cons b a))))
    (mapcar #'reverse (reduce #'-reduce l
                              :initial-value (loop repeat (length (first l))
                                                   collect (list))))))


(declaim (inline list>than))
(defun list>than (l n)
  (declare (list l) (pos-int n))
  "list is longer than n?"
  (consp (nthcdr n l)))


(defmacro with-linspace ((n a b rn &key (end t) collect) &body body)
  (declare (symbol rn))
  (alexandria:with-gensyms (a* b* n* nn i ba)
  `(let* ((,n* ,n)
          (,nn (coerce (if ,end (1- ,n*) ,n*) 'double-float))
          (,a* ,a)
          (,b* ,b)
          (,ba (- ,b* ,a*)))
    (declare (pos-int ,n*) (double-float ,nn ,a* ,b* ,ba))
    (loop for ,i from 0 below ,n* ,(if collect 'collect 'do)
      (let ((,rn (+ ,a* (* (coerce ,i 'double-float) (/ ,ba ,nn)))))
        (declare (double-float ,rn))
        (progn ,@body))))))


(declaim (inline linspace))
(defun linspace (n a b &key (end t))
  (declare #.*opt-settings* (pos-int n) (double-float a b) (boolean end))
  "n double-floats from a to b."
  (if (> n 1)
    (loop with ban of-type double-float = (/ (- b a) (if end (1- n) n))
          for i of-type fixnum from 0 below n
          collect (+ a (* (coerce i 'double-float) ban)) of-type double-float)
    (list a)))


(declaim (inline lerp))
(defun lerp (s a b)
  (declare #.*opt-settings* (double-float s a b))
  "
  linear interpolation between a and b.
  if 0 < s < 1 the result is between a and b
  "
  (+ a (* s (- b a))))

(declaim (inline llerp))
(defun llerp (s ab)
  (declare #.*opt-settings* (double-float s) (list ab))
  "list of interpolations on ab"
  (apply #'lerp s ab))

; LIST MATH


(defmacro lop (name type &body body)
  `(defun ,name (aa bb)
     (declare #.*opt-settings* (list aa bb))
     (loop for a of-type ,type in aa and b of-type ,type in bb
           collect (the ,type (,@body (the ,type a) (the ,type b)))
             of-type ,type)))


(declaim (inline add sub mult dadd dsub dmult ddiv))
(lop add fixnum +)
(lop sub fixnum -)
(lop mult fixnum *)
(lop dadd double-float +)
(lop dsub double-float -)
(lop dmult double-float *)
(lop ddiv double-float /)


(declaim (inline 3cross))
(defun 3cross (a1 a2 a3 b1 b2 b3)
  (declare #.*opt-settings* (double-float a1 a2 a3 b1 b2 b3))
  (values (- (* a2 b3) (* a3 b2))
          (- (* a3 b1) (* a1 b3))
          (- (* a1 b2) (* a2 b1))))


(declaim (inline imod))
(defun imod (i inc m)
  (declare #.*opt-settings* (fixnum i inc m))
  (mod (+ i inc) m))

(declaim (inline dmod))
(defun dmod (i inc m)
  (declare #.*opt-settings* (double-float i inc m))
  (mod (+ i inc) m))

(declaim (inline mod2))
(defun mod2 (i)
  (declare #.*opt-settings* (fixnum i))
  (mod i 2))


(declaim (inline ddst))
(defun ddst (aa bb)
  (declare #.*opt-settings* (list aa bb))
  (sqrt (the double-float (loop for a in aa and b in bb
                                sum (expt (the double-float (- a b)) 2d0)
                                  of-type double-float ))))

(declaim (inline dsum))
(defun dsum (aa)
  (declare #.*opt-settings* (list aa))
  (loop for a of-type double-float in aa summing a of-type double-float))

(declaim (inline dmean))
(defun dmean (aa)
  (declare #.*opt-settings* (list aa))
  (/ (dsum aa) (coerce (length aa) 'double-float)))


; OTHER


(defun copy-sort (a fx &key (key #'identity))
  (declare (sequence a))
  (sort (copy-seq a) fx :key key))


(defun percentiles (aa)
  (declare (list aa))
  (let ((n (length aa))
        (percentiles (list 0.05d0 0.1d0 0.5d0 0.9d0 0.95d0))
        (srt (make-adjustable-vector :init (copy-sort aa #'>))))
    (to-vector
      (append (list (aref srt 0))
              (loop for m in percentiles collect (aref srt (floor (* n m))))
              (list (vector-last srt))))))


(defun range-search (ranges f &aux (n (1- (length ranges)))
                                   (ranges* (ensure-vector ranges)))
  "
  binary range search.
  range must be sorted in ascending order. f is a value inside the range you
  are looking for.
  "
  (if (or (< f (aref ranges* 0)) (> f (aref ranges* n)))
    (error "querying position outside range: ~a" f))

  (loop with l of-type fixnum = 0
        with r of-type fixnum = n
        with m of-type fixnum = 0
        until (<= (aref ranges* m) f (aref ranges* (1+ m)))
        do (setf m (floor (+ l r) 2))
           (cond ((> f (aref ranges* m))
                   (setf l (progn m)))
                 ((< f (aref ranges* m))
                   (setf r (1+ m))))
        finally (return m)))


(defun integer-search (aa v &aux (n (length aa)))
  (declare #.*opt-settings* (sequence aa) (fixnum v n))
  "binary integer search. assumes presorted list of integers"
  (loop with l of-type fixnum = 0
        with r of-type fixnum = (1- n)
        with m of-type fixnum = 0
        while (<= l r)
        do (setf m (ceiling (+ l r) 2))
           (cond ((< (aref aa m) v)
                   (setf l (1+ m)))
                 ((> (aref aa m) v)
                   (setf r (1- m)))
                 (t (return-from integer-search m)))))


(defun argmax (ll &optional (key #'identity))
  (declare (list ll) (function key))
  (loop with iv = 0
        with v = (funcall key (first ll))
        for l in (cdr ll)
        and i from 1
        if (> (funcall key l) v)
        do (setf v (funcall key l) iv i)
        finally (return (list iv v))))


(defun argmin (ll &optional (key #'identity))
  (declare (list ll) (function key))
  (loop with iv = 0
        with v = (funcall key (first ll))
        for l in (cdr ll)
        and i from 1
        if (< (funcall key l) v)
        do (setf v (funcall key l) iv i)
        finally (return (list iv v))))

