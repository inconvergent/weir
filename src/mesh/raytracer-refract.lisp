(in-package :mesh)

(defvar *air-medium-eta* (/ 1d0 1.5d0))

(declaim (inline -swap-state))
(defun -swap-state (s)
  (declare #.*opt-settings* (symbol s))
  (case s (:inside :outside)
          (:outside :inside)))

(declaim (inline -get-eta))
(defun -get-eta (state eta)
  (declare #.*opt-settings* (symbol state) (double-float eta))
  (case state (:inside (/ eta))
              (t eta)))

(declaim (inline refract-or-reflect))
(defun refract-or-reflect (normal ray &key (eta *air-medium-eta*)
                                            (state :outside) (len 5000d0))
  (declare #.*opt-settings* (list ray) (vec:3vec normal) (symbol state)
                            (double-float eta len))
  (let* ((pt (second ray))
         (rd (-get-direction ray))
         (n (if (< (vec:3dot normal rd) 0d0) normal (vec:3neg normal))))
    (multiple-value-bind (r xref) (vec:3refract rd n (-get-eta state eta))
      (declare (vec:3vec r) (boolean xref))
      (if xref
          ; refract (exit medium)
          (values (list pt (vec:3from pt (vec:3norm r) len))
                  (-swap-state state))
          ; reflect internally
          (values (list pt (vec:3from pt (vec:3norm-reflect rd n) len))
                  state)))))

(defun make-refraction-raytracer (raycastfx &key include-state (len 5000d0)
                                                 (num 20))
  (declare #.*opt-settings* (function raycastfx) (boolean include-state)
                            (double-float len) (pos-int num))
  "
  make naive refraction raytracer that traces the path of single rays.  this
  function assumes that the ray starts on the 'outside' of the mesh and
  switches state if the ray is transmittet (not reflected totally.)
  is it correct? who knows.
  "
  (let ((update (if include-state
                    (lambda (res v s)
                      (declare (list res) (vec:3vec v) (symbol s))
                      (cons (list v s) res))
                    (lambda (res v s)
                      (declare (list res) (vec:3vec v) (ignore s))
                      (cons v res)))))
    (labels
      ((raytracer (start &key (state :outside) (eta *air-medium-eta*))
        (declare #.*opt-settings* (list start) (symbol state)
                                  (double-float eta len))
        (loop with res of-type list = (list)
              with ray of-type list = (copy-list start)
              ; TODO: detect state?
              with state* of-type symbol = state
              with prev of-type list = *nilpoly*
              for i of-type pos-int from 0 below num
              initially (setf res (funcall update res (first start) state*))
              do (let ((hit (funcall raycastfx ray :skip prev)))
                   (declare (bvhres hit))
                   ; nothing happens if there are no ray hits
                   (when (equal (bvhres-i hit) '(-1 -1 -1))
                         (when (> i 0)
                               (setf res (funcall update res (second ray) state*)))
                         (return-from raytracer res))
                   ; refract or reflect
                   (let ((pt (bvhres-pt hit)))
                     (multiple-value-bind (new-ray new-state)
                       (refract-or-reflect (bvhres-n hit) (-make-new-ray ray pt)
                         :state state* :eta eta :len len)
                       (setf res (funcall update res pt new-state))
                       (setf ray new-ray
                             prev (sort (the list (bvhres-i hit)) #'<)
                             state* new-state))))
              finally (return-from raytracer res))))
      #'raytracer)))

