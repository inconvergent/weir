
(in-package :graph)

"
a simple (undirected) graph structure based on adjacency lists.
"


(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))


(defvar *inf* 1d8)


(defstruct (graph (:constructor -make-graph))
  (size 0 :type pos-int :read-only t)
  (num-edges 0 :type pos-int)
  (adj nil :type hash-table)
  (make-hset #'identity :type function :read-only t))


(defun make (&key (adj-size 4) (adj-inc 2f0)
                  (set-size 10) (set-inc 2f0))
  (declare #.*opt-settings*)
  (-make-graph :num-edges 0
               :adj (make-hash-table :test #'eql :size adj-size
                                     :rehash-size adj-inc)
               :make-hset (lambda (x) (declare (inline))
                            (hset:make :init x :size set-size :inc set-inc))))

(defun copy (grph)
  (declare #.*opt-settings* (graph grph))
  ; :key is called in the value before setting
  ; https://common-lisp.net/project/alexandria/draft/alexandria.html#Hash-Tables
  ; TODO: handle adj-size, set-size, set-inc, adj-inc across graph struct
  (-make-graph :num-edges (graph-num-edges grph)
               :adj (alexandria:copy-hash-table (graph-adj grph)
                      :key #'hset:copy)
               :make-hset (graph-make-hset grph)))


(declaim (inline -add))
(defun -add (makefx adj a b)
  (declare #.*opt-settings* (function makefx) (pos-int a b))
  (multiple-value-bind (val exists) (gethash a adj)
    (if (not exists)
        (progn (setf val (funcall makefx (list b))
                         (gethash a adj) val)
               t)
        (hset:add val b))))


(defun add (grph a b)
  (declare #.*opt-settings* (graph grph) (pos-int a b))
  (with-struct (graph- adj make-hset) grph
    (declare (function make-hset))
    (let ((ab (-add make-hset adj a b))
          (ba (-add make-hset adj b a)))
      (declare (boolean ab ba))
      (when (or ab ba) (incf (graph-num-edges grph) 2)
                       t))))


(declaim (inline -del))
(defun -del (adj a b)
  (declare #.*opt-settings* (pos-int a b))
  (multiple-value-bind (val exists) (gethash a adj)
    (when exists (hset:del val b))))


(declaim (inline -prune))
(defun -prune (adj a)
  (declare #.*opt-settings* (pos-int a))
  (multiple-value-bind (val exists) (gethash a adj)
    (when (and exists (< (the pos-int (hset:num val)) 1))
          (remhash a adj))))


(defun del (grph a b)
  (declare #.*opt-settings* (graph grph) (pos-int a b))
  (with-struct (graph- adj) grph
    (let ((ab (-del adj a b))
          (ba (-del adj b a)))
      (declare (boolean ab ba))
      (when (or ab ba) (-prune adj a)
                       (-prune adj b)
                       (decf (graph-num-edges grph) 2)
                       t))))


(defun get-num-edges (grph)
  (declare #.*opt-settings* (graph grph))
  (/ (graph-num-edges grph) 2))

(defun get-num-verts (grph)
  (declare #.*opt-settings* (graph grph))
  (hash-table-count (graph-adj grph)))


(defun mem (grph a b)
  (declare #.*opt-settings* (graph grph) (pos-int a b))
  (with-struct (graph- adj) grph
    (multiple-value-bind (val exists) (gethash a adj)
      (when exists (hset:mem val b)))))


(defun get-edges (grph)
  (declare #.*opt-settings* (graph grph))
  (loop with res of-type list = (list)
        with adj of-type hash-table = (graph-adj grph)
        for a of-type pos-int being the hash-keys of adj
        do (loop for b of-type pos-int being the hash-keys of (gethash a adj)
                 if (< a b) do (push (list a b) res))
        finally (return res)))


(defun get-verts (grph)
  (declare #.*opt-settings* (graph grph))
  (loop for v being the hash-keys of (graph-adj grph)
          using (hash-value ee)
        collect v))


(defun get-incident-edges (grph v)
  (declare #.*opt-settings* (graph grph) (pos-int v))
  (with-struct (graph- adj) grph
    (let ((a (gethash v adj)))
      (when a (loop for w of-type pos-int being the hash-keys of a
                    collect (sort (list v w) #'<))))))

(defun -only-incident-verts (v ee)
  (declare (pos-int v) (list ee))
  (remove-if (lambda (i) (= i v)) (alexandria:flatten ee)))

(defun get-incident-verts (grph v)
  (declare (graph grph) (pos-int v))
  (-only-incident-verts v (get-incident-edges grph v)))


(defun vmem (grph v)
  (declare #.*opt-settings* (graph grph) (pos-int v))
  (if (gethash v (graph-adj grph)) t nil))


(defmacro with-graph-edges ((grph e) &body body)
  (alexandria:with-gensyms (adj a b)
    `(loop with ,e of-type list
           with ,adj of-type hash-table = (graph-adj ,grph)
           for ,a of-type pos-int being the hash-keys of ,adj collect
      (loop for ,b of-type pos-int being the hash-keys of (gethash ,a ,adj)
            if (< ,a ,b)
            do (setf ,e (list ,a ,b))
               (progn ,@body)))))

