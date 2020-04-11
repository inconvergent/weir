(in-package :mesh)


(declaim (inline -make-poly-fx-tuples))
(defun -make-poly-fx-tuples (msh vertfx)
  (declare (mesh msh) (function vertfx))
  (mapcar (lambda (poly)
            (declare (list poly))
            (let ((vv (funcall vertfx poly)))
              (declare (list vv))
              (list poly (apply #'vec::3make-polyx vv)
                         (apply #'-poly-normal vv))))
          (get-all-polygons msh)))


(defun make-raycaster (msh &key (vertfx (lambda (poly) (declare (list poly))
                                          (get-verts msh poly)))
                                (resfx #'first))
  (declare #.*opt-settings* (mesh msh) (function vertfx resfx))
  (let ((poly-fx (-make-poly-fx-tuples msh vertfx)))
    (declare (list poly-fx))
    (lambda (line &key (skip *nilpoly*))
      (declare #.*opt-settings* (list line skip))
      (loop with org of-type vec:3vec = (first line)
            with l of-type vec:3vec = (apply #'vec:3isub line)
            with res of-type list = (list)
            for (poly fx normal) in poly-fx
            do (multiple-value-bind (isect s p) (funcall (the function fx) org l)
                 ;(declare (boolean isect) (vec:3vec p) (double-float s))
                 (when (and isect (not (equal skip poly)))
                       (push (list s p poly normal) res)))
            finally (return (funcall resfx
                              (sort res #'< :key #'first)))))))


(defun make-hidden-fx (msh &key (vertfx (lambda (poly) (declare (list poly))
                                          (get-verts msh poly))))
  (declare (mesh msh) (function vertfx))
  (let ((poly-fx (-make-poly-fx-tuples msh vertfx)))
    (declare (list poly-fx))
    (lambda (line)
      (declare (list line))
      (loop with org of-type vec:3vec = (first line)
            with l of-type vec:3vec = (apply #'vec:3isub line)
            for (poly fx _) in poly-fx
            if (funcall (the function fx) org l)
            do (return t)
            finally (return nil)))))


(declaim (inline -get-direction))
(defun -get-direction (line)
  (declare #.*opt-settings* (list line))
  (vec:3norm! (apply #'vec:3isub line)))


(declaim (inline -make-new-ray))
(defun -make-new-ray (ray pt)
  (declare #.*opt-settings* (list ray) (vec:3vec pt))
  (list (first ray) pt))


(declaim (inline -reflect))
(defun -reflect (normal ray &key (len 5000d0))
  (declare #.*opt-settings* (list ray) (double-float len))
  (let* ((rd (-get-direction ray))
         (pt (second ray))
         (n (if (< (vec:3dot normal rd) 0d0) normal (vec:3neg normal))))
    (declare (vec:3vec n pt rd))
    (list pt (vec:3from pt (vec:3norm-reflect rd n) len))))

(defun make-raytracer (raycastfx &key (len 5000d0) (num 5))
  (declare #.*opt-settings* (function raycastfx)
                            (double-float len) (pos-int num))
  "
  make simple raytracer. must initiate raycastfx with resfx=#'first.
  supports bvh-raycaster and (brute force) raycaster.
  "
  (labels
    ((raytracer (start)
      (declare #.*opt-settings* (list start))
      (loop with res of-type list = (list)
            with update of-type function = (lambda (v) (declare (vec:3vec v))
                                             (push v res))
            with ray of-type list = (copy-list start)
            with prev of-type list = *nilpoly*
            for i of-type pos-int from 0 below num
            initially (funcall update (first start))
            do (let ((isect (funcall raycastfx ray :skip prev)))
                (declare (list isect))
                ; nothing happens if there are no ray hits
                (when (not isect)
                      (when (> i 0) (funcall update (second ray)))
                      (return-from raytracer res))
                ; reflect
                (destructuring-bind (_ pt new-poly normal) isect
                  (declare (ignore _) (vec:3vec pt) (list new-poly))
                  (let ((new-ray (-reflect normal (-make-new-ray ray pt)
                                           :len len)))
                    (declare (list new-ray))
                    (funcall update pt)
                    (setf ray new-ray prev new-poly))))
            finally (return-from raytracer res))))
    #'raytracer))

