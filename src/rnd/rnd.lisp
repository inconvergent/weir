
(in-package :rnd)

(deftype pos-double () `(double-float 0d0 *))

(defconstant PII  (* PI 2d0))


(defun set-rnd-state (i)
  (declare (fixnum i))
   #+SBCL
   (setf *random-state* (sb-ext:seed-random-state i))

   #+(not SBCL)
   (warn "rnd:state is only implemented for SBCL. see src/rnd.lisp
          to implement state for your environment."))


(defun make-rnd-state ()
  (setf *random-state* (make-random-state t)))


; NUMBERS AND RANGES

(declaim (inline rndi))
(defun rndi (a)
  (declare (fixnum a))
  (the fixnum (random a)))

(declaim (inline nrndi))
(defun nrndi (n a)
  (declare (fixnum n a))
  (loop repeat n collect (rndi a) of-type fixnum))


(declaim (inline rndrngi))
(defun rndrngi (a b)
  (declare (fixnum a b))
  (+ a (rndi (- b a))))

(declaim (inline nrndrngi))
(defun nrndrngi (n a b)
  (declare (fixnum n a b))
  (let ((d (- b a)))
    (declare (fixnum d))
    (loop repeat n collect (+ a (rndi d)) of-type fixnum)))


(declaim (inline rnd))
(defun rnd (&optional (x 1d0))
  (declare #.*opt-settings* (double-float x))
  (random x))

(declaim (inline nrnd))
(defun nrnd (n &optional (x 1d0))
  (declare #.*opt-settings* (fixnum n) (double-float x))
  (loop repeat n collect (rnd x) of-type double-float))


(declaim (inline rnd*))
(defun rnd* (&optional (x 1d0))
  (declare #.*opt-settings* (double-float x))
  (- x (rnd (* 2d0 x))))

(declaim (inline nrnd*))
(defun nrnd* (n &optional (x 1d0))
  (declare #.*opt-settings* (fixnum n) (double-float x))
  (loop repeat n collect (rnd* x) of-type double-float))


(declaim (inline rndrng))
(defun rndrng (a b)
  (declare #.*opt-settings* (double-float a b))
  (+ a (rnd (- b a))))

(declaim (inline nrndrng))
(defun nrndrng (n a b)
  (declare #.*opt-settings* (fixnum n) (double-float a b))
  (loop repeat n collect (rndrng a b) of-type double-float))


; https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
(declaim (inline norm))
(defun norm (&key (mu 0d0) (sigma 1d0))
  "box-muller transform"
  (declare (double-float mu sigma))
  (let ((s (* sigma (the double-float
                         (sqrt (the pos-double
                                    (* -2d0 (log (rnd))))))))
        (u (* PII (rnd))))
    (declare (double-float s u))
    (values (+ mu (* s (cos u)))
            (+ mu (* s (sin u))))))


; MACROS


(defmacro prob (p a &optional b)
  "executes body with probability p"
  `(if (< (rnd) (the double-float ,p)) ,a ,b))


(defmacro either (a b)
  "excecutes either a or b, with a probablility of 0.5"
  `(prob 0.5d0 ,a ,b))


(defmacro rcond (&rest clauses)
  "
  executes the forms in clauses according to the weighted sum of
  all p1, p2 ...
  clauses should be on this form:
    ((p1 form) (p2 form) ...)
  "
  (alexandria:with-gensyms (val)
    (let* ((tot 0d0)
           (clauses* (loop for (p . body) in clauses
                           do (incf tot (the double-float p))
                           collect `((< ,val ,tot) ,@body))))
      (declare (double-float tot) (list clauses*))
      `(let ((,val (rnd ,tot)))
         (declare (double-float ,val))
         (cond ,@clauses*)))))


(defmacro rep (n &body body)
  "repeat body at most n-1 times"
  `(loop repeat (rndi (the fixnum ,n)) do (progn ,@body)))


(defmacro reprng (a b &body body)
  "repeat body between [a b) times"
  `(loop repeat (rndrngi ,a ,b) do (progn ,@body)))


; TODO: refactor
(defmacro with-rndspace ((n a b rn &key collect) &body body)
  "repeat body where rn is n numbers between (a b)"
  (declare (symbol rn))
  (alexandria:with-gensyms (d a*)
    `(let* ((,a* ,a)
            (,d (- ,b ,a*)))
      (declare (double-float ,a* ,d))
      (loop repeat ,n ,(if collect 'collect 'do)
        (let ((,rn (+ ,a* (rnd ,d))))
          (declare (double-float ,rn))
          (progn ,@body))))))


; GENERIC

(defun rndget (l)
  (declare #.*opt-settings* (sequence l))
  (if (eql (type-of l) 'cons) (nth (rndi (length l)) l)
                              (aref l (rndi (length l)))))

(declaim (inline probsel))
(defun probsel (p a &aux (a* (ensure-vector a)))
  (declare #.*opt-settings* (sequence a) (double-float p))
  (loop with res of-type vector = (make-adjustable-vector)
        for e across a*
        do (prob p (vextend e res))
        finally (return res)))


(defmacro -nrep (n &body body)
  (alexandria:with-gensyms (nname)
    `(let ((,nname ,n))
      (loop repeat ,nname collect (progn ,@body)))))


(declaim (inline rndspace))
(defun rndspace (n a b &key order &aux (d (- b a)))
  (declare #.*opt-settings* (fixnum n) (double-float a b d))
  (if order (sort (the list (-nrep n (+ a (rnd d)))) #'<)
            (-nrep n (+ a (rnd d)))))


(declaim (inline rndspacei))
(defun rndspacei (n a b &key order &aux (d (- b a)))
  (declare #.*opt-settings* (fixnum n a b d))
  (if order (sort (-nrep n (+ a (rndi d))) #'<)
            (-nrep n (+ a (rndi d)))))


(declaim (inline bernoulli))
(defun bernoulli (n p)
  (declare #.*opt-settings* (fixnum n) (double-float p))
  (loop repeat n collect (prob p 1d0 0d0) of-type double-float))

