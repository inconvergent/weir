
(in-package :vec)

(deftype pos-double () `(double-float 0d0 *))


(declaim (inline make-vec vec-x vec-y))
(defstruct (vec)
  (x 0d0 :type double-float :read-only nil)
  (y 0d0 :type double-float :read-only nil))


(declaim (inline make-3vec 3vec-x 3vec-y 3vec-z))
(defstruct (3vec (:include vec))
  (z 0d0 :type double-float :read-only nil))

(declaim (inline vec))
(defun vec (x y)
  (declare (double-float x y))
  (make-vec :x x :y y))

(declaim (inline 3vec))
(defun 3vec (x y z)
  (declare (double-float x y z))
  (make-3vec :x x :y y :z z))

(declaim (inline set!))
(defun set! (a b)
  (setf (vec-x a) (vec-x b)
        (vec-y a) (vec-y b))
  a)

(declaim (inline 3set!))
(defun 3set! (a b)
  (setf (3vec-x a) (3vec-x b)
        (3vec-y a) (3vec-y b)
        (3vec-z a) (3vec-z b))
  a)


(defmacro rep (&body body)
  `(vec (progn ,@body) (progn ,@body)))


(defmacro with-xy ((v x y) &body body)
  (declare (symbol x y))
  (alexandria:with-gensyms (vname)
    `(let* ((,vname ,v)
            (,x (vec-x ,vname))
            (,y (vec-y ,vname)))
      (declare (double-float ,x ,y))
      (progn ,@body))))


(defmacro vec-op ((name &key inplace) &body fx)
  (alexandria:with-gensyms (a b)
    `(defun ,name (,a ,b)
      (declare #.*opt-settings* (vec ,a ,b))
      ,(if inplace
         `(progn
            (setf (vec-x ,a) (,@fx (vec-x ,a) (vec-x ,b))
                  (vec-y ,a) (,@fx (vec-y ,a) (vec-y ,b)))
            ,a)
         `(vec (,@fx (vec-x ,a) (vec-x ,b))
               (,@fx (vec-y ,a) (vec-y ,b)))))))

(defmacro scalar-op ((name &key inplace) &body fx)
  (alexandria:with-gensyms (a s)
    `(defun ,name (,a ,s)
      (declare #.*opt-settings* (vec ,a) (double-float ,s))
      ,(if inplace
         `(progn
            (setf (vec-x ,a) (,@fx (vec-x ,a) ,s)
                  (vec-y ,a) (,@fx (vec-y ,a) ,s))
            ,a)
         `(vec (,@fx (vec-x ,a) ,s)
               (,@fx (vec-y ,a) ,s))))))


(defmacro list-vec-op ((name &key inplace) &body fx)
  (alexandria:with-gensyms (aa bb)
    `(defun ,name (,aa ,bb)
      (declare #.*opt-settings* (list ,aa ,bb))
      ,(if inplace `(progn (map nil #',@fx ,aa ,bb) ,aa)
                   `(mapcar #',@fx ,aa ,bb)))))


(defmacro broadcast-vec-op ((name &key itr-type res-type arg-types inplace) &body fx)
  (let* ((pairs (loop for type in arg-types collect (list type (gensym))))
         (args (loop for (_ a) in pairs collect a)))
    (alexandria:with-gensyms (aa a)
      `(defun ,name (,aa ,@args)
        (declare #.*opt-settings* (list ,aa) ,@pairs)
        ,(if inplace
             `(loop for ,a of-type ,itr-type in ,aa
                    do (,@fx ,a ,@args)
                    finally (return ,aa))
             `(loop for ,a of-type ,itr-type in ,aa
                    collect (,@fx ,a ,@args) of-type ,res-type))))))


(defmacro 3rep (&body body)
  `(3vec (progn ,@body) (progn ,@body) (progn ,@body)))


(declaim (inline zero))
(defun zero ()
  (declare #.*opt-settings*)
  (rep 0d0))

(declaim (inline 3zero))
(defun 3zero ()
  (declare #.*opt-settings*)
  (3rep 0d0))


(defmacro 3with-xy ((v x y z) &body body)
  (declare (symbol x y z))
  (alexandria:with-gensyms (vname)
    `(let* ((,vname ,v)
            (,x (3vec-x ,vname))
            (,y (3vec-y ,vname))
            (,z (3vec-z ,vname)))
      (declare (double-float ,x ,y ,z))
      (progn ,@body))))


(defmacro 3vec-op ((name &key inplace) &body fx)
  (alexandria:with-gensyms (a b)
    `(defun ,name (,a ,b)
      (declare #.*opt-settings* (vec ,a ,b))
      ,(if inplace
         `(progn
            (setf (3vec-x ,a) (,@fx (3vec-x ,a) (3vec-x ,b))
                  (3vec-y ,a) (,@fx (3vec-y ,a) (3vec-y ,b))
                  (3vec-z ,a) (,@fx (3vec-z ,a) (3vec-z ,b)))
            ,a)
         `(3vec (,@fx (3vec-x ,a) (3vec-x ,b))
                (,@fx (3vec-y ,a) (3vec-y ,b))
                (,@fx (3vec-z ,a) (3vec-z ,b)))))))

(defmacro 3scalar-op ((name &key inplace) &body fx)
  (alexandria:with-gensyms (a s)
    `(defun ,name (,a ,s)
      (declare #.*opt-settings* (vec ,a) (double-float ,s))
      ,(if inplace
         `(progn
            (setf (3vec-x ,a) (,@fx (3vec-x ,a) ,s)
                  (3vec-y ,a) (,@fx (3vec-y ,a) ,s)
                  (3vec-z ,a) (,@fx (3vec-z ,a) ,s))
            ,a)
         `(3vec (,@fx (3vec-x ,a) ,s)
                (,@fx (3vec-y ,a) ,s)
                (,@fx (3vec-z ,a) ,s))))))



(declaim (inline -add))
(defun -add (a b)
  (declare #.*opt-settings* (double-float a b))
  (+ a b))

(declaim (inline -mult))
(defun -mult (a b)
  (declare #.*opt-settings* (double-float a b))
  (* a b))

(declaim (inline -isub))
(defun -isub (a b)
  (declare #.*opt-settings* (double-float a b))
  (- b a))

(declaim (inline -sub))
(defun -sub (a b)
  (declare #.*opt-settings* (double-float a b))
  (- a b))

(declaim (inline -idiv))
(defun -idiv (a b)
  (declare #.*opt-settings* (double-float a b))
  (/ b a))

(declaim (inline -div))
(defun -div (a b)
  (declare #.*opt-settings* (double-float a b))
  (/ a b))

(declaim (inline -expt))
(defun -expt (a b)
  (declare #.*opt-settings* (double-float a b))
  (expt (the pos-double a) b))


(declaim (inline add sub mult div isub
                 add! sub! mult! div! isub!
                 sadd ssub smult sdiv
                 sadd! ssub! smult! sdiv!
                 ladd lsub lmult ldiv lisub
                 ladd! lsub! lmult! ldiv! lisub!
                 ladd* lsub* lmult* ldiv* lisub*
                 lsmult*))

(vec-op (add) -add)
(vec-op (add! :inplace t) -add)
(scalar-op (sadd) -add)
(scalar-op (sadd! :inplace t) -add)
(list-vec-op (ladd) add)
(list-vec-op (ladd! :inplace t) add!)
(broadcast-vec-op (ladd* :itr-type vec :res-type vec :arg-types (vec)) add)
(broadcast-vec-op (ladd!* :itr-type vec :res-type vec :arg-types (vec) :inplace t) add!)

(vec-op (sub) -sub)
(vec-op (sub! :inplace t) -sub)
(scalar-op (ssub) -sub)
(scalar-op (ssub! :inplace t) -sub)
(list-vec-op (lsub) sub)
(list-vec-op (lsub! :inplace t) sub!)
(broadcast-vec-op (lsub* :itr-type vec :res-type vec :arg-types (vec)) sub)
(broadcast-vec-op (lsub!* :itr-type vec :res-type vec :arg-types (vec) :inplace t) sub!)

(vec-op (mult) -mult)
(vec-op (mult! :inplace t) -mult)
(scalar-op (smult) -mult)
(scalar-op (smult! :inplace t) -mult)
(list-vec-op (lmult) mult)
(list-vec-op (lmult! :inplace t) mult!)
(broadcast-vec-op (lmult* :itr-type vec :res-type vec :arg-types (vec)) mult)
(broadcast-vec-op (lmult!* :itr-type vec :res-type vec :arg-types (vec) :inplace t) mult!)
(broadcast-vec-op (lsmult* :itr-type vec :res-type vec :arg-types (double-float)) smult)
(broadcast-vec-op (lsmult!* :itr-type vec :res-type vec :arg-types (double-float) :inplace t) smult!)

(vec-op (div) -div)
(vec-op (div! :inplace t) -div)
(scalar-op (sdiv) -div)
(scalar-op (sdiv! :inplace t) -div)
(list-vec-op (ldiv) div)
(list-vec-op (ldiv! :inplace t) div!)
(broadcast-vec-op (ldiv* :itr-type vec :res-type vec :arg-types (vec)) div)
(broadcast-vec-op (ldiv!* :itr-type vec :res-type vec :arg-types (vec) :inplace t) div!)

(vec-op (idiv) -idiv)
(vec-op (idiv! :inplace t) -idiv)
(list-vec-op (lidiv) idiv)
(list-vec-op (lidiv! :inplace t) idiv!)
(broadcast-vec-op (lidiv* :itr-type vec :res-type vec :arg-types (vec)) idiv)
(broadcast-vec-op (lidiv!* :itr-type vec :res-type vec :arg-types (vec) :inplace t) idiv!)

(vec-op (isub) -isub)
(vec-op (isub! :inplace t) -isub)
(list-vec-op (lisub) isub)
(list-vec-op (lisub! :inplace t) isub!)
(broadcast-vec-op (lisub* :itr-type vec :res-type vec :arg-types (vec)) isub)
(broadcast-vec-op (lisub!* :itr-type vec :res-type vec :arg-types (vec) :inplace t) isub!)


(declaim (inline 3add 3sub 3mult 3div 3isub
                 3add! 3sub! 3mult! 3div! 3isub!
                 3sadd 3ssub 3smult 3sdiv
                 3sadd! 3ssub! 3smult! 3sdiv!
                 3ladd 3lsub 3lmult 3ldiv 3lisub
                 3ladd* 3lsub* 3lmult* 3ldiv* 3lisub*
                 3lsmult*))

(3vec-op (3add) -add)
(3vec-op (3add! :inplace t) -add)
(3scalar-op (3sadd) -add)
(3scalar-op (3sadd! :inplace t) -add)
(list-vec-op (3ladd) 3add)
(list-vec-op (3ladd! :inplace t) 3add!)
(broadcast-vec-op (3ladd* :itr-type 3vec :res-type 3vec :arg-types (3vec)) 3add)
(broadcast-vec-op (3ladd!* :itr-type 3vec :res-type 3vec :arg-types (3vec) :inplace t) 3add!)

(3vec-op (3sub) -sub)
(3vec-op (3sub! :inplace t) -sub)
(3scalar-op (3ssub) -sub)
(3scalar-op (3ssub! :inplace t) -sub)
(list-vec-op (3lsub) 3sub)
(list-vec-op (3lsub! :inplace t) 3sub!)
(broadcast-vec-op (3lsub* :itr-type 3vec :res-type 3vec :arg-types (3vec)) 3sub)
(broadcast-vec-op (3lsub!* :itr-type 3vec :res-type 3vec :arg-types (3vec) :inplace t) 3sub!)

(3vec-op (3mult) -mult)
(3vec-op (3mult! :inplace t) -mult)
(3scalar-op (3smult) -mult)
(3scalar-op (3smult! :inplace t) -mult)
(list-vec-op (3lmult) 3mult)
(list-vec-op (3lmult! :inplace t) 3mult!)
(broadcast-vec-op (3lmult* :itr-type 3vec :res-type 3vec :arg-types (3vec)) 3mult)
(broadcast-vec-op (3lmult!* :itr-type 3vec :res-type 3vec :arg-types (3vec) :inplace t) 3mult!)
(broadcast-vec-op (3lsmult* :itr-type 3vec :res-type 3vec :arg-types (double-float)) 3smult)
(broadcast-vec-op (3lsmult!* :itr-type 3vec :res-type 3vec :arg-types (double-float) :inplace t) 3smult!)

(3vec-op (3div) -div)
(3vec-op (3div! :inplace t) -div)
(3scalar-op (3sdiv) -div)
(3scalar-op (3sdiv! :inplace t) -div)
(list-vec-op (3ldiv) 3div)
(list-vec-op (3ldiv! :inplace t) 3div!)
(broadcast-vec-op (3ldiv* :itr-type 3vec :res-type 3vec :arg-types (3vec)) 3div)
(broadcast-vec-op (3ldiv!* :itr-type 3vec :res-type 3vec :arg-types (3vec) :inplace t) 3div!)

(3vec-op (3idiv) -idiv)
(3vec-op (3idiv! :inplace t) -idiv)
(list-vec-op (3lidiv) 3idiv)
(list-vec-op (3lidiv! :inplace t) 3idiv!)
(broadcast-vec-op (3lidiv* :itr-type 3vec :res-type 3vec :arg-types (3vec)) 3idiv)
(broadcast-vec-op (3lidiv!* :itr-type 3vec :res-type 3vec :arg-types (3vec) :inplace t) 3idiv!)

(3vec-op (3isub) -isub)
(3vec-op (3isub! :inplace t) -isub)
(list-vec-op (3lisub) 3isub)
(list-vec-op (3lisub! :inplace t) 3isub!)
(broadcast-vec-op (3lisub* :itr-type 3vec :res-type 3vec :arg-types (3vec)) 3isub)
(broadcast-vec-op (3lisub!* :itr-type 3vec :res-type 3vec :arg-types (3vec) :inplace t) 3isub!)

