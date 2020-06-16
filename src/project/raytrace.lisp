
(in-package :ortho)


(defun gauss (foc ex mult &key (mi 0d0))
  (lambda (&key d xy)
    (declare (inline) (vec:vec xy) (double-float d))
    (multiple-value-bind (a b) (rnd:norm)
      (declare (double-float a b))
      (vec:from xy (vec:vec a b)
                   (max mi (* mult (expt (abs (- foc d)) ex)))))))


; TODO: shift from middle of cell
(defun make-ray-shift (proj &key (s 0.5d0))
  (declare #.*opt-settings* (ortho proj) (double-float s))
  (let* ((invs (/ (ortho-s proj)))
         (us (vec:3smult (ortho-u proj) invs))
         (vs (vec:3smult (ortho-v proj) invs)))
    (declare (double-float invs) (vec:3vec us vs))
    (lambda (ray)
      (declare (list ray))
      (let ((shift (vec:3add! (vec:3smult us (rnd:rnd* s))
                              (vec:3smult vs (rnd:rnd* s)))))
        (declare (vec:3vec shift))
        (mapcar (lambda (a) (vec:3add a shift)) ray)))))


(defun make-sample-disc-ray (proj &key rad d (len 5000d0))
  (declare #.*opt-settings* (ortho proj) (double-float rad d len))
  (let ((su (vec:3smult (ortho-u proj) rad))
        (sv (vec:3smult (ortho-v proj) rad))
        (fp (vec:3from (vec:3zero) (ortho-vpn proj) (- d)))) ; focal point
    (declare (vec:3vec su sv fp))
    (lambda (xy)
      (declare (vec:3vec xy))
      (multiple-value-bind (a b) (rnd:norm)
        (declare (double-float a b))
        (let* ((r (vec:3add! (vec:3smult su a) (vec:3smult sv b))) ; disc sample
               (srd (vec:3norm! (vec:3sub fp r) :s len)) ; secondary ray direction
               (pt (vec:3add! r xy))) ; ray start
          (declare (vec:3vec r srd pt))
          (list pt (vec:3add pt srd)))))))


(defun parallel-pixel-render (proj size renderfx &key (parts 40))
  (declare #.*opt-settings* (ortho proj) (pos-int parts size)
                            (function renderfx))
  "
  execute (renderfx i j xy) for every pixel (i j) with world coordinate xy
  "
  (with-struct (ortho- u v s xy cam) proj
    (declare (vec:3vec u v cam) (double-float s) (vec:vec xy))
    (let ((invs (/ s)))
      (declare (double-float invs))
      (vec:with-xy ((vec:smult xy (- invs)) px py)
        (labels
          ((-pt (uv u px i)
             (declare #.*opt-settings* (double-float px i) (vec:3vec u uv))
             (vec:3add! (vec:3smult u (+ px (* i invs))) uv))
           (-do-row (j)
             (declare #.*opt-settings* (fixnum j))
             (loop with j* of-type double-float = (coerce j 'double-float)
                   with uv of-type vec:3vec = (-pt cam v py j*)
                   for i of-type fixnum from 0 below size
                   do (funcall renderfx i j
                               (-pt uv u px (coerce i 'double-float))))))
          (lparallel:pmap nil #'-do-row :parts parts
            (to-vector (math:range 0 size))))))))


(defun -pixel-render (proj size renderfx)
  (declare #.*opt-settings* (ortho proj) (pos-int size)
                            (function renderfx))
  "
  execute (renderfx i j xy) for every pixel (i j) with world coordinate xy
  "
  (with-struct (ortho- u v s xy cam) proj
    (declare (vec:3vec u v cam) (double-float s) (vec:vec xy))
    (let ((invs (/ s)))
      (declare (double-float invs))
      (vec:with-xy ((vec:smult xy (- invs)) px py)
        (labels
          ((-pt (uv u px i)
             (declare #.*opt-settings* (double-float px i) (vec:3vec u uv))
             (vec:3add! (vec:3smult u (+ px (* i invs))) uv))
           (-do-row (j)
             (declare #.*opt-settings* (fixnum j))
             (loop with j* of-type double-float = (coerce j 'double-float)
                   with uv of-type vec:3vec = (-pt cam v py j*)
                   for i of-type fixnum from 0 below size
                   do (funcall renderfx i j
                               (-pt uv u px (coerce i 'double-float))))))
          (map nil #'-do-row (to-vector (math:range 0 size))))))))

