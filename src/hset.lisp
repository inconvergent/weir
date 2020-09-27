
(in-package :hset)

"
fixnum set

this is a naive wrapper around hash-map. not sure how efficient it will be?
"


(defun make (&key init (size 100) (inc 2f0))
  (declare #.*opt-settings* (fixnum size))
  (let ((s (make-hash-table :test #'eql :size size :rehash-size inc)))
    (when init (add* s init))
    s))


(defun copy (s &key (size 100) (inc 2f0))
  (declare #.*opt-settings* (fixnum size))
  (let ((ns (make-hash-table :test #'eql :size size :rehash-size inc)))
    (loop for k being the hash-keys of s
          do (setf (gethash k ns) t))
    ns))


(declaim (inline add))
(defun add (s e)
  (declare #.*opt-settings* (fixnum e))
  (multiple-value-bind (val exists) (gethash e s)
    (declare (ignore val))
    (if exists nil (setf (gethash e s) t))))


(defun add* (s ee)
  (declare #.*opt-settings* (hash-table s) (sequence ee))
  (if (equal (type-of ee) 'cons)
      (loop for e of-type fixnum in ee collect (add s e))
      (loop for e of-type fixnum across ee collect (add s e))))


(defun del (s e)
  (declare (hash-table s) (fixnum e))
  (declare #.*opt-settings* (fixnum e))
  (remhash e s))


(defun del* (s ee)
  (declare #.*opt-settings* (hash-table s) (sequence ee))
  (if (equal (type-of ee) 'cons)
    (loop for e of-type fixnum in ee collect (remhash e s))
    (loop for e of-type fixnum across ee collect (remhash e s))))


(declaim (inline mem))
(defun mem (s e)
  (declare #.*opt-settings* (hash-table s) (fixnum e))
  (multiple-value-bind (_ exists) (gethash e s)
    (declare (ignore _))
    exists))


(defun mem* (s ee)
  (declare #.*opt-settings* (hash-table s) (list ee))
  (loop for e of-type fixnum in ee collect
    (multiple-value-bind (v exists) (gethash e s)
      (declare (ignore v))
      exists)))


(defun num (s)
  (declare #.*opt-settings* (hash-table s))
  (the fixnum (hash-table-count s)))


(defun to-list (s)
  (declare #.*opt-settings* (hash-table s))
  (loop for e of-type fixnum being the hash-keys of s collect e))


; SET OPS (not tested)

(defun uni (a b)
  (declare #.*opt-settings* (hash-table a b))
  (let ((c (copy a)))
    (loop for k being the hash-keys of b
          do (setf (gethash k c) t))
    c))

(defun inter (a b)
  (declare #.*opt-settings* (hash-table a b))
  (loop with c = (make)
        for k being the hash-keys of a
        do (when (mem b k)
                 (setf (gethash k c) t))
        finally (return c)))

(defun symdiff (a b)
  (declare #.*opt-settings* (hash-table a b))
  (let ((un (uni a b)))
    (loop for k being the hash-keys of un
          do (when (and (mem a k) (mem b k))
                   (remhash k un)))
    un))

