
(defpackage :convex
  (:use :common-lisp)
  (:export split)
  (:import-from :common-lisp-user
    :make-adjustable-vector
    :to-vector
    :vextend))


(in-package :convex)


(defun -get-splits (n pts &aux (n- (1- n)))
  (let ((len (loop for i from 0 below (1- n) and ii from 1
                   summing (vec:dst (aref pts i) (aref pts ii)))))
    (flet ((lenok (i) (< (rnd:rnd)
                         (/ (vec:dst (aref pts i) (aref pts (1+ i))) len))))
      (loop with a with b
            do (setf a (rnd:rndi n-)
                     b (rnd:rndi n-))
            until (and (not (= a b))
                       (funcall #'lenok a)
                       (funcall #'lenok b))
            finally (return (sort (list a b) #'<))))))


(defun -split-get-left (n a b)
  (let ((res (make-adjustable-vector)))
    (loop for i from 0
          while (<= i a)
          do (vextend i res))
    (vextend (list a (1+ a)) res)
    (vextend (list b (1+ b)) res)
    (loop for i from (1+ b)
          while (< i n)
          do (vextend (mod i (1- n)) res))
    res))


(defun -split-get-right (n a b)
  (let ((res (make-adjustable-vector)))
    (loop for i from (1+ a)
          while (<= i b)
          do (vextend i res))
    (vextend (list b (1+ b)) res)
    (vextend (list a (1+ a)) res)
    (loop for i from (1+ a)
          while (<= (mod i n) (1+ a))
          do (vextend i res))
    res))


(defun -split-ind-to-pts (pts inds s)
  (to-vector (loop for i across inds
                   collect (if (eql (type-of i) 'cons)
                             (destructuring-bind (a b)
                               (mapcar (lambda (i*) (aref pts i*)) i)
                               (vec:from a (vec:sub b a) s))
                             (aref pts i)))))


(defun split (pts &key (s 0.5d0) (lim 0d0) &aux (n (length pts)))
  (if (< (loop for i from 0 below (1- n)
               minimizing (vec:dst (aref pts i) (aref pts (1+ i)))) lim)
      (return-from split (list pts nil)))

  (destructuring-bind (a b) (-get-splits n pts)
    (list (-split-ind-to-pts pts (-split-get-left n a b) s)
          (-split-ind-to-pts pts (-split-get-right n a b) s))))

