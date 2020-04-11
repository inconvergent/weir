(in-package :point-cloud)


;(declaim (inline -get-direction))
;(defun -get-direction (line)
;  (declare #.*opt-settings* (list line))
;  (vec:3norm! (apply #'vec:3isub line)))


;(declaim (inline -make-new-ray))
;(defun -make-new-ray (ray pt)
;  (declare #.*opt-settings* (list ray) (vec:3vec pt))
;  (list (first ray) pt))


;(declaim (inline -reflect))
;(defun -reflect (normal ray &key (len 5000d0))
;  (declare #.*opt-settings* (list ray) (double-float len))
;  (let* ((rd (-get-direction ray))
;         (pt (second ray))
;         (n (if (< (vec:3dot normal rd) 0d0) normal (vec:3neg normal))))
;    (declare (vec:3vec n pt rd))
;    (list pt (vec:3from pt (vec:3norm-reflect rd n) len))))

;(defun make-raytracer (raycastfx &key (len 5000d0) (num 5))
;  (declare #.*opt-settings* (function raycastfx)
;                            (double-float len) (pos-int num))
;  "
;  make simple raytracer. must initiate raycastfx with resfx=#'first.
;  supports bvh-raycaster.
;  "
;  (labels
;    ((raytracer (start)
;      (declare #.*opt-settings* (list start))
;      (loop with res of-type list = (list)
;            with update of-type function = (lambda (v) (declare (vec:3vec v))
;                                             (push v res))
;            with ray of-type list = (copy-list start)
;            with prev of-type list = *nilpt*
;            for i of-type pos-int from 0 below num
;            initially (funcall update (first start))
;            do (let ((isect (funcall raycastfx ray :skip prev)))
;                (declare (list isect))
;                ; nothing happens if there are no ray hits
;                (when (not isect)
;                      (when (> i 0) (funcall update (second ray)))
;                      (return-from raytracer res))
;                ; reflect
;                (destructuring-bind (_ pt new-poly normal) isect
;                  (declare (ignore _) (vec:3vec pt) (list new-poly))
;                  (let ((new-ray (-reflect normal (-make-new-ray ray pt)
;                                           :len len)))
;                    (declare (list new-ray))
;                    (funcall update pt)
;                    (setf ray new-ray prev new-poly))))
;            finally (return-from raytracer res))))
;    #'raytracer))

