
(in-package :curvature)

(deftype pos-double () `(double-float 0d0 *))

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))

(deftype vec-simple () `(simple-array vec:vec))


;TODO: add first order estimations on boundary points?
(declaim (inline ddxy))
(defun ddxy (pts i &aux (n (length pts)))
  "
  calculate first and second derivatives of pts wrt t
  (the parametrisation variable).
  assumes that pts is parameterised from 0 to 1, and evenly sampled in t.
  "
  (declare #.*opt-settings* (vec-simple pts) (pos-int i))
  (let* ((s (/ 2d0 (coerce (1- n) 'double-float)))
         (s2 (expt s 2d0))
         (p (aref pts i))
         (p- (aref pts (1- i)))
         (p+ (aref pts (1+ i))))
    (declare (vec:vec p- p+ p) (pos-double s s2))
    (values (/ (- (vec:vec-x p+) (vec:vec-x p-)) s)
            (/ (- (vec:vec-y p+) (vec:vec-y p-)) s)
            (/ (+ (vec:vec-x p+) (vec:vec-x p-) (* -2d0 (vec:vec-x p))) s2)
            (/ (+ (vec:vec-y p+) (vec:vec-y p-) (* -2d0 (vec:vec-y p))) s2))))


(declaim (inline kappa))
(defun kappa (pts i)
  "estimate curvature based on pts"
  (declare #.*opt-settings* (vec-simple pts) (pos-int i))
  (multiple-value-bind (dx dy ddx ddy) (ddxy pts i)
    (declare (double-float dx dy ddx ddy))
    (/ (abs (- (* dx ddy) (* dy ddx)))
       (expt (the pos-double (+ (* dx dx) (* dy dy)))
             #.(/ 3d0 2d0)))))


(declaim (inline -coffset))
(defun -coffset (pts angles i)
  (declare #.*opt-settings* (vec-simple pts) (vec-simple angles)
                            (pos-int i))
  (let* ((va (aref angles (1- i)))
         (vb (aref angles i))
         (ab (vec:angle vb)))
    (declare (vec:vec va vb) (double-float ab))
    (list (kappa pts i) (aref pts i)
          (if (<= (the double-float (vec:cross va vb)) 0d0)
              (vec:cos-sin (+ ab PI5))
              (vec:cos-sin (- ab PI5))))))

(declaim (inline -pad-offsets))
(defun -pad-offsets (pts)
  (declare #.*opt-settings* (list pts))
  (concatenate 'list (list (first pts)) pts (last pts)))


; TODO: closed version
(defun offsets (pts)
  "
  offset pts according to estimated curvature.
  pts must be evenly distributed
  "
  (declare #.*opt-settings* (vec-simple pts))
  (-pad-offsets
    (loop with angles of-type vec-simple = (math:path-angles pts)
          for i of-type pos-int from 1 below (1- (length pts))
          collect (-coffset pts angles i ))))


(declaim (inline -do-split-num))
(defun -do-split-num (res rs curvefx curr c a b)
  (declare #.*opt-settings* (list res) (pos-double rs) (double-float c)
                            (function curvefx) (pos-int curr) (vec:vec a b))
  (let* ((cw (funcall curvefx c))
         (n (ceiling (the pos-double (* rs cw)))))
    (declare (double-float cw) (pos-int n))
    (when (math:list>than res 0) (push (list n cw a b) (first res)))
    (when (not (= n curr)) (push (list (list n cw a b)) res))
    (values res n)))

(declaim (inline -split-num))
(defun -split-num (offsets rs curvefx)
  (declare #.*opt-settings* (list offsets) (pos-double rs) (function curvefx))
  (loop with curr of-type pos-int = 0
        with res of-type list = (list)
        for (c a b) in offsets
        do (multiple-value-bind (res* n)
             (-do-split-num res rs curvefx curr c a b)
             (declare (list res) (pos-int n))
             (setf curr n res res*))
        finally (return res)))


(declaim (inline -curvefx))
(defun -curvefx (c)
  (declare (pos-double c))
  (* 500d0 (expt c 0.6d0)))

(declaim (inline -spacefx))
(defun -spacefx (n)
  (declare (pos-int n))
  (math:linspace n 0d0 1d0))

; TODO: closed
(defun offset-paths (pts &key (rs 0.1d0) (curvefx #'-curvefx)
                                         (spacefx #'-spacefx))
  "
  offset pts according to curvature.
  pts must be evenly sampled for this to work properly.
  experimental.
  "
  (declare #.*opt-settings* (vec-simple pts) (pos-double rs)
                            (function curvefx spacefx))
  (loop with res of-type list = (list)
        for offset of-type list in (-split-num (offsets pts) rs curvefx)
        do (loop with n of-type pos-int = (caar offset)
                 for s of-type pos-double in (funcall spacefx n)
                 do (push
                      (list n (loop for (_ c a b) in offset
                                    collect (vec:on-line s a (vec:from a b c))
                                      of-type vec:vec))
                      res))
        finally (return res)))

