
(in-package :cpath)


(defparameter *clim* -0.5d0)
(defparameter *slim* -0.95d0)


(defun -scale-offset (w a b &key (fxn #'sin))
  (declare #.*opt-settings*
           (double-float w) (vec:vec a b) (function fxn))
  (let ((s (abs (funcall fxn (abs (- (vec:angle a) (vec:angle b)))))))
    (declare (double-float s))
    (if (< s 0.05d0) w (/ w s))))


(defun -offset (v o)
  (list (vec:add v o) (vec:sub v o)))


(defun -chamfer (w diag pa na aa aa-)
  (let* ((x (< (vec:cross aa aa-) 0d0))
         (corner (if x (second diag) (first diag)))
         (s (-scale-offset w aa- na :fxn #'cos)))
    (loop for v in (-offset pa (vec:smult (vec:perp na) s))
          collect (if x (list v corner) (list corner v)))))

(defun -regular-perp (a b)
  (declare #.*opt-settings* (vec:vec a b))
  (vec:perp (vec:norm (vec:add a b))))


(defun -make-test-fxn-closed (angles clim slim)
  (declare #.*opt-settings*
           (type (simple-array vec:vec) angles) (double-float clim slim))
  (let ((n- (1- (length angles))))
    (lambda (i) (declare (fixnum i))
      (let ((a (aref angles i))
            (a- (aref angles (if (< i 1) n- (1- i)))))
        (let ((dt (vec:dot a- a)))
          (cond ((<= dt slim) (values :sharp (vec:perp a-)))
                ((<  dt clim) (values :chamfer (-regular-perp a- a)))
                (t (values :regular (-regular-perp a- a)))))))))


(defun -make-test-fxn-open (angles clim slim)
  (declare #.*opt-settings*
           (type (simple-array vec:vec) angles) (double-float clim slim))
  (let ((n- (1- (length angles))))
    (lambda (i) (declare (fixnum i))
      (let ((a (aref angles i)))
        (if (> n- i 0)
          (let ((dt (vec:dot (aref angles (1- i)) a)))
            (cond ((<= dt slim) (values :sharp (vec:perp (aref angles (1- i)))))
                  ((< dt clim) (values :chamfer (-regular-perp
                                                  (aref angles (1- i)) a)))
                  (t (values :regular (-regular-perp (aref angles (1- i)) a)))))
          (values :regular (vec:perp a)))))))


(defun -width-fx (widths)
  (if (equal (type-of widths) 'double-float)
      (lambda (i) (declare (ignore i)) widths)
      (let ((w (ensure-vector widths)))
        (lambda (i) (aref w i)))))


(defun get-diagonals (pts widths clim slim closed)
  (let* ((res (make-adjustable-vector))
         (n (length pts))
         (angles (math:path-angles pts))
         (get-width (-width-fx widths))
         (corner-test (if closed (-make-test-fxn-closed angles clim slim)
                                 (-make-test-fxn-open angles clim slim))))

    (loop for i from 0 below (if closed (1- n) n) do
      (multiple-value-bind (corner na) (funcall (the function corner-test) i)
        (let* ((p (aref pts i))
               (a (aref angles i))
               (w (funcall get-width i))
               (diag (-offset p (vec:smult na (-scale-offset w a na)))))
          (mapcar (lambda (d) (vextend d res))
                  (case corner
                    (:chamfer (-chamfer w diag p na a
                                (aref angles (math:imod i -1 n))))
                    (:regular (list diag))
                    (:sharp (list (progn diag) (reverse diag))))))))

    ;handle last closed path diagonal
    (when closed (vextend (aref res 0) res))
    res))


(defun -get-ind (i k i- closed)
  (if closed i (if (= (math:mod2 k) 0) i i-)))


(defun cpath (pts widths rep &key (slim *slim*) (clim *clim*) closed)
  (loop with diagonals = (get-diagonals pts widths clim slim closed)
        with n = (length diagonals)
        with res of-type vector = (make-adjustable-vector)
        for s of-type double-float in (math:linspace rep 0d0 1d0)
        and k of-type fixnum from 0
        do (loop for i of-type fixnum from 0 below n
                 and i- of-type fixnum downfrom (1- n)
                 do (vextend (vec:lon-line s (aref diagonals (-get-ind i k i- closed)))
                             res))
    finally (return (to-list res))))


(defun outline (pts widths &key closed (clim *clim*) (slim *slim*))
  (declare (list pts))
  (let* ((pts* (to-vector (if closed (math:close-path pts) pts))))
    (math:close-path (cpath pts* widths 2 :closed closed
                            :clim clim :slim slim))))


(defun path-offset (pts widths &key (s 1d0) closed (clim *clim*) (slim *slim*))
  (let ((diag (get-diagonals (to-vector pts) widths clim slim closed)))
    (loop for d across diag collect (vec:lon-line s d))))

