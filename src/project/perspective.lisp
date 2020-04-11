
(in-package :perspective)

"Naive 3d->2d projection that uses three 2d vectors (a, b, c) as well as center
(xy) and scale (s) for projection"


(declaim (inline -make-perspective))
(defstruct (perspective (:constructor -make-perspective))
  (a nil :type vec:vec :read-only t)
  (b nil :type vec:vec :read-only t)
  (c nil :type vec:vec :read-only t)
  (xy nil :type vec:vec :read-only t)
  (s 1d0 :type double-float :read-only t))


(defun -length-norm (ll)
  (declare (list ll))
  (loop with len of-type double-float =
          (loop for p of-type vec:vec in ll
                sum (vec:len p) of-type double-float)
        for p of-type vec:vec in ll
        collect (vec:smult p (/ len))))

(defun make* (abc &key (xy vec:*zero*) (s 1d0))
  (declare (list abc))
  (destructuring-bind (a b c) (-length-norm abc)
    (-make-perspective :a a :b b :c c :xy xy :s s)))

(defun make (a b c &key (xy vec:*zero*) (s 1d0))
  (declare (vec:vec a b c))
  (destructuring-bind (a* b* c*) (-length-norm (list a b c))
    (-make-perspective :a a* :b b* :c c* :xy xy :s s)))


(declaim (inline -project))
(defun -project (a b c s xy pt)
  (declare #.*opt-settings*
           (vec:vec a b c xy) (double-float s) (vec:3vec pt))
  (vec:3with-xy (pt x y z)
    (vec:add xy (vec:lsum (list (vec:smult a (* s x))
                                (vec:smult b (* s y))
                                (vec:smult c (* s z)))))))


(defun project (p pt &key xy s)
  (declare #.*opt-settings*
           (perspective p))
  (with-struct (perspective- a b c) p
    (let ((xy* (if xy xy (perspective-xy p)))
          (s* (if s s (perspective-s p))))
      (declare (double-float s*) (vec:vec xy*))
      (if (equal (type-of pt) 'cons)
        (loop for p of-type vec:3vec in pt collect (-project a b c s* xy* p))
        (-project a b c s* xy* pt)))))


(defun get-projector (&key noise (xy vec:*zero*) (s 1d0)
                                 (angle #.(/ (* PI 7d0) 6d0)))
  (make* (loop for a in (math:linspace 3 angle (+ angle PII) :end nil)
               collect (vec:cos-sin (+ a (if noise (rnd:rnd* noise) 0d0))))
         :xy xy :s s))

