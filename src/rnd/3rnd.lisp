
(in-package :rnd)


(defun 3on-line (a b)
  (declare #.*opt-settings* (vec:3vec a b))
  (vec:3from a (vec:3sub b a) (rnd)))

(defun 3on-line* (ab)
  (declare #.*opt-settings* (list ab))
  (apply #'3on-line ab))


(defun 3non-line (n a b)
  (declare #.*opt-settings* (fixnum n) (vec:3vec a b))
  (loop with ba = (vec:3sub b a)
        repeat n
        collect (vec:3from a ba (rnd))))

(defun 3non-line* (n ab)
  (declare #.*opt-settings* (fixnum n) (list ab))
  (apply #'3non-line n ab))


(declaim (inline -3in-box))
(defun -3in-box (sx sy sz)
  (declare #.*opt-settings* (double-float sx sy sz))
  (vec:3vec (rnd* sx) (rnd* sy) (rnd* sz)))

(defun 3in-box (sx sy sz &key (xy vec:*3zero*))
  (declare #.*opt-settings* (double-float sx sy sz) (vec:3vec xy))
  (vec:3add! (-3in-box sx sy sz) xy))

(defun 3in-cube (s &key (xy vec:*3zero*))
  (declare #.*opt-settings* (double-float s) (vec:3vec xy))
  (vec:3add! (-3in-box s s s) xy))


(defun 3nin-box (n sx sy sz &key (xy vec:*3zero*))
  (declare #.*opt-settings* (fixnum n) (double-float sx sy sz) (vec:3vec xy))
  (loop repeat n collect (3in-box sx sy sz :xy xy)))

(defun 3nin-cube (n s &key (xy vec:*3zero*))
  (declare #.*opt-settings* (fixnum n) (double-float s) (vec:3vec xy))
  (loop repeat n collect (3in-cube s :xy xy)))


(declaim (inline -3norm))
(defun -3norm (a b c)
  (declare #.*opt-settings* (double-float a b c))
  (sqrt (+ (* a a) (* b b) (* c c))))

; TODO: efficient non-sphere
(declaim (inline 3on-sphere))
(defun 3on-sphere (&key (rad 1d0) (xy vec:*3zero*))
  (declare #.*opt-settings* (double-float rad) (vec:3vec xy))
  (multiple-value-bind (a b) (norm)
    (declare (double-float a b))
    (let* ((c (norm))
           (l (/ rad (-3norm a b c))))
      (declare (double-float c l))
      (vec:3vec (+ (vec:3vec-x xy) (* a l))
                (+ (vec:3vec-y xy) (* b l))
                (+ (vec:3vec-z xy) (* c l))))))


(declaim (inline 3in-sphere))
(defun 3in-sphere (&key (rad 1d0) (xy vec:*3zero*))
  (declare #.*opt-settings* (double-float rad) (vec:3vec xy))
  (loop with cand of-type vec:3vec = (-3in-box 1d0 1d0 1d0)
        until (< (vec:3len2 cand) 1d0)
        do (setf (vec:3vec-x cand) (rnd*) (vec:3vec-y cand) (rnd*)
                 (vec:3vec-z cand) (rnd*))
        finally (setf (vec:3vec-x cand) (+ (vec:3vec-x xy)
                                           (* (vec:3vec-x cand) rad))
                      (vec:3vec-y cand) (+ (vec:3vec-y xy)
                                           (* (vec:3vec-y cand) rad))
                      (vec:3vec-z cand) (+ (vec:3vec-z xy)
                                           (* (vec:3vec-z cand) rad)))
                (return cand)))

