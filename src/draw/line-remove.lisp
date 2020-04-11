
(in-package :line-remove)

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))


(defstruct line-remove
  (counts nil :type hash-table :read-only t)
  (cnt 0 :type pos-int :read-only t)
  (rs 0d0 :type double-float :read-only t)
  (is 0d0 :type double-float :read-only t)
  (min-length 0d0 :type double-float :read-only t)
  (total-length 0d0 :type double-float :read-only nil)
  (saved-length 0d0 :type double-float :read-only nil))


(defun make (&key (cnt 4) (is 1d0) (rs 1.5d0) (min-length 1d0))
  (declare (pos-int cnt) (double-float is rs min-length))
  (when (> is rs) (print "WARN: is should be smaller than rs"))
  (make-line-remove :counts (make-hash-table :test #'equal)
                    :rs rs :cnt cnt :is is :min-length min-length))


(defun stats (lr)
  (let ((sl (line-remove-saved-length lr))
        (tl (line-remove-total-length lr)))
    (declare (double-float sl tl))
    (list :saved-length sl :total-length tl
          :ratio (if (> tl 0d0) (/ sl tl) 0d0))))


(declaim (inline -linearize-line))
(defun -linearize-line (n is a b)
  (declare #.*opt-settings* (pos-int n) (double-float is) (vec:vec a b))
  (math:with-linspace (n 0d0 1d0 s :collect t)
    (list s (vec:smult (vec:on-line s a b) is))))


(declaim (inline -distinct-inds))
(defun -distinct-inds (xy)
  (declare #.*opt-settings* (vec:vec xy))
  (vec:with-xy (xy x y)
    (list (list #1=(floor x) #2=(floor y))
          (list (1+ #1#) #2#)
          (list #1# (1+ #2#))
          (list (1+ #1#) (1+ #2#)))))


(declaim (inline -do-inc))
(defun -do-inc (hits counts xy)
  (declare #.*opt-settings* (hash-table hits counts) (vec:vec xy))
  (loop for ind of-type list in (-distinct-inds xy)
        do (unless (gethash ind hits) (setf (gethash ind hits) t)
                                      (incf (gethash ind counts 0)))))


(declaim (inline -get-count))
(defun -get-count (counts xy)
  (declare #.*opt-settings* (hash-table counts) (vec:vec xy))
  (loop for ind of-type list in (-distinct-inds xy)
        maximize (gethash ind counts 0) of-type fixnum))


(defun -do-count (counts is rs a b)
  (declare #.*opt-settings* (hash-table counts) (double-float is rs) (vec:vec a b))
  (let* ((d (vec:dst a b))
         (lin (-linearize-line (max 2 (ceiling (* rs d))) is a b))
         (hits (make-hash-table :test #'equal))
         (res (loop for (s xy) in lin collect (list s (-get-count counts xy)))))
    (loop for (s xy) of-type (double-float vec:vec) in lin
          do (-do-inc hits counts xy))
    res))


(declaim (inline -find-nils))
(defun -find-nils (pts cnt)
  (declare #.*opt-settings* (list pts) (pos-int cnt))
  (loop with res of-type vector = (make-adjustable-vector)
        for (_ c) in pts
        and i of-type fixnum from 0
        do (when (> c cnt) (vextend i res))
        finally (return res)))


(declaim (inline -proc))
(defun -proc (pts cnt min-length len)
  (declare #.*opt-settings* (pos-int cnt) (double-float min-length len))
  (let ((pts* (to-vector pts))
        (nils (-find-nils pts cnt)))
    (declare (vector pts*) (sequence nils))
    (when (< (length nils) 1) (return-from -proc '((0d0 1d0))))
    (setf nils (concatenate 'list (list -1) nils (list (length pts*))))
    (remove-if (lambda (ss) (declare (list ss))
                 (< (* len (abs (apply #'- ss))) min-length))
               (loop for a of-type fixnum in nils
                     and b of-type fixnum in (cdr nils)
                     if (> (- b a) 2)
                     collect (list (first (aref pts* (1+ a)))
                                   (first (aref pts* (1- b))))))))


(defun path-split (lr pts)
  "
  remove sections of lines that cover too crowded places.
  "
  (declare #.*opt-settings* (line-remove lr) (sequence pts))
  (with-struct (line-remove- counts cnt is rs min-length) lr
    (loop with res of-type vector = (make-adjustable-vector)
          for path of-type list across (ensure-vector pts)
          do (loop with drawn-length of-type double-float = 0d0
                   with total-length of-type double-float = 0d0
                   for a in path and b in (cdr path)
                   do (let* ((len (vec:dst a b))
                             (segments (-proc (-do-count counts is rs a b)
                                              cnt min-length len)))
                        (incf total-length len)
                        (when segments
                          (loop for (si sj) in segments
                                do (let ((p (vec:on-line si a b))
                                         (q (vec:on-line sj a b)))
                                     (incf drawn-length (vec:dst p q))
                                     (vextend (list p q) res)))))
                   finally (incf (line-remove-total-length lr) total-length)
                           (incf (line-remove-saved-length lr)
                                 (- total-length drawn-length)))
          finally (return res))))

