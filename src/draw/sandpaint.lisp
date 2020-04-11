
(in-package :sandpaint)

(deftype small-int (&optional (size 30000))
  `(integer 0 ,size))

(deftype pos-int (&optional (bits 31))
  `(unsigned-byte ,bits))

(deftype pos-double () `(double-float 0d0 *))


(declaim (inline -vfloor*))
(defun -vfloor* (v)
  (declare #.*opt-settings* (vec:vec v))
  (values (the fixnum (floor (vec:vec-x v)))
          (the fixnum (floor (vec:vec-y v)))))


(defmacro -inside-floor ((size xy x y) &body body)
  (declare (symbol x y))
  (alexandria:with-gensyms (sname)
    `(let ((,sname ,size))
      (declare (small-int ,sname))
      (multiple-value-bind (,x ,y) (-vfloor* ,xy)
        (declare (fixnum ,x ,y))
        (when (and (< -1 ,x ,sname) (< -1 ,y ,sname))
              (progn ,@body))))))


(defmacro -square-loop ((x y n) &body body)
  (declare (symbol x y n))
  (alexandria:with-gensyms (nname)
    `(let ((,nname ,n))
      (loop for ,y of-type small-int from 0 below ,nname
            do (loop for ,x of-type small-int from 0 below ,nname
                     do (progn ,@body))))))


(defmacro -do-op ((sand size vals indfx &key name) &body body)
  (declare (symbol sand size vals indfx))
  (alexandria:with-gensyms (sname)
    `(let* ((,sname ,sand)
            (,size (sandpaint-size ,sname))
            (,vals (sandpaint-vals ,sname))
            (,indfx (sandpaint-indfx ,sname)))
      (declare (type (simple-array double-float) ,vals)
               (function ,indfx) (small-int ,size))
      ,(when name `(format t "applying:~a...~%" ,name))
      (progn ,@body))))


(declaim (inline -indfx))
(defun -indfx (s x y c)
  (declare #.*opt-settings* (small-int s x y c))
  (+ c (the pos-int (* 4 (the pos-int (+ x (the pos-int (* s y))))))))

(declaim (inline -get-indfx))
(defun -get-indfx (size)
  (declare (small-int size))
  (lambda (x y &optional (c 0))
    (declare #.*opt-settings* (small-int x y c))
    (-indfx size x y c)))


(defstruct sandpaint
  (size nil :type small-int :read-only t)
  (vals nil :type (simple-array double-float) :read-only t)
  (fg nil :type pigment:rgba :read-only nil)
  (bg nil :type pigment:rgba :read-only nil)
  (indfx nil :type function :read-only t))


(defun make-rgba-array (size &key (init 0d0))
  (declare (small-int size))
  (make-array (* size size 4) :adjustable nil
                              :initial-element init
                              :element-type 'double-float))


(defun -rgb-from (vals ind &optional (a 1d0))
  (declare #.*opt-settings*
           (pos-int ind)
           (type (simple-array double-float) vals))
  (pigment:rgb (aref vals ind) (aref vals (1+ ind)) (aref vals (+ ind 2)) a))


(declaim (inline sample))
(defun sample (sand xy &key (alpha 1d0))
  (-do-op (sand size vals indfx)
    (-inside-floor (size xy x y)
      (let* ((ind (funcall indfx x y))
             (a (aref vals (+ ind 3))))
        (declare (pos-int ind) (double-float a))
        (pigment:rgb (/ (aref vals ind) a)
                     (/ (aref vals (1+ ind)) a)
                     (/ (aref vals (+ ind 2)) a)
                     alpha)))))


(declaim (inline set-pix))
(defun set-pix (sand i j c)
  (declare #.*opt-settings* (pos-int i j) (pigment:rgba c))
  (-do-op (sand size vals indfx)
    (let ((ind (funcall indfx i j)))
      (declare (pos-int ind))
      (setf (aref vals ind) (pigment::rgba-r c)
            (aref vals (1+ ind)) (pigment::rgba-g c)
            (aref vals (+ ind 2)) (pigment::rgba-b c)
            (aref vals (+ ind 3)) (pigment::rgba-a c))
      nil)))


(defun get-size (sand) (sandpaint-size sand))


(declaim (inline -scale-convert))
(defun -scale-convert (v &key (s 1d0) (gamma 1d0))
  (declare (double-float s gamma))
  (setf v (expt (the pos-double (max 0d0 (/ v s))) gamma)))


(declaim (inline -operator-over))
(defun -operator-over (indfx vals x y fg)
  (declare #.*opt-settings* (small-int x y)
           (pigment:rgba fg) (function indfx)
           (type (simple-array double-float) vals))
  (let* ((ind (funcall indfx x y))
         (a (pigment::rgba-a fg))
         (ia (- 1d0 a)))
    (declare (pos-int ind) (double-float a ia))
    (setf (aref vals ind) (+ (* (aref vals ind) ia) (pigment::rgba-r fg))
          (aref vals (incf ind)) (+ (* (aref vals ind) ia) (pigment::rgba-g fg))
          (aref vals (incf ind)) (+ (* (aref vals ind) ia) (pigment::rgba-b fg))
          (aref vals (incf ind)) (+ (* (aref vals ind) ia) a))))


(declaim (inline -floor-fract))
(defun -floor-fract (pt)
  (declare #.*opt-settings*)
  (vec:with-xy (pt x y)
    (multiple-value-bind (ix fx) (floor x)
      (declare (fixnum ix) (double-float fx))
      (multiple-value-bind (iy fy) (floor y)
        (declare (fixnum iy) (double-float fy))
        (values ix iy fx fy)))))

(declaim (inline -fract-overlap))
(defun -fract-overlap (x y)
  (declare #.*opt-settings* (double-float x y))
  (let ((x2 (- 1d0 x))
        (y2 (- 1d0 y)))
    (declare (double-float x2 y2))
    (values (* x2 y2) (* x y2) (* x2 y) (* x y))))


;suggested by
;https://twitter.com/porglezomp/status/1014612499315003392
(declaim (inline -pix-overlap))
(defun -pix-overlap (indfx vals size pt fg)
  (declare #.*opt-settings* (vec:vec pt) (pigment:rgba fg)
                            (function indfx)
                            (small-int size)
                            (type (simple-array double-float) vals))
  (labels
    ((operator-over-overlap (ix iy s)
      (declare (fixnum ix iy) (double-float s))
      (when (and (< -1 ix size) (< -1 iy size))
            (-operator-over indfx vals ix iy (pigment:scale fg s)))))

      (multiple-value-bind (ix iy fx fy) (-floor-fract pt)
        (declare (fixnum ix iy) (double-float fx fy))
        (multiple-value-bind (s1 s2 s3 s4) (-fract-overlap fx fy)
          (declare (double-float s1 s2 s3 s4))
          (operator-over-overlap ix iy s1)
          (operator-over-overlap (+ ix 1) iy s2)
          (operator-over-overlap ix (+ iy 1) s3)
          (operator-over-overlap (+ ix 1) (+ iy 1) s4)))))


(declaim (inline -draw-stroke))
(defun -draw-stroke (indfx vals size grains fg v1 v2)
  (declare (function indfx)
           (pigment:rgba fg)
           (type (simple-array double-float) vals)
           (fixnum grains) (small-int size))
  (rnd:with-on-line (grains v1 v2 rn)
    (-inside-floor (size rn x y)
      (-operator-over indfx vals x y fg))))


(declaim (inline -draw-stroke-overlap))
(defun -draw-stroke-overlap (indfx vals size grains fg v1 v2)
  (declare (function indfx)
           (pigment:rgba fg)
           (type (simple-array double-float) vals)
           (fixnum grains) (small-int size))
  (rnd:with-on-line (grains v1 v2 pt)
    (-pix-overlap indfx vals size pt fg)))


(declaim (inline -draw-dens-stroke))
(defun -draw-dens-stroke (indfx vals size dens fg v1 v2)
  (declare #.*opt-settings*
           (function indfx)
           (type (simple-array double-float) vals)
           (small-int size) (double-float dens)
           (pigment:rgba fg))
  (rnd:with-on-line ((ceiling (* dens (vec:dst v1 v2))) v1 v2 rn)
    (-inside-floor (size rn x y)
      (-operator-over indfx vals x y fg))))


(declaim (inline -draw-circ))
(defun -draw-circ (indfx vals size xy rad grains fg)
  (declare #.*opt-settings*
           (function indfx)
           (type (simple-array double-float) vals)
           (fixnum grains) (small-int size)
           (double-float rad) (pigment:rgba fg))
  (rnd:with-in-circ (grains rad p :xy xy)
    (-inside-floor (size p x y)
      (-operator-over indfx vals x y fg))))


(declaim (inline -u8))
(defun -u8 (v)
  (declare #.*opt-settings* (double-float v))
  (cond ((>= v 1d0) 255)
        ((<= v 0d0) 0)
        (t (floor (* 255d0 v)))))

(declaim (inline -ui8))
(defun -ui8 (v)
  (declare #.*opt-settings* (fixnum v))
  (cond ((>= v 255) 1d0)
        ((<= v 0) 0d0)
        (t (/ (coerce v 'double-float) 255d0))))

(declaim (inline -u16))
(defun -u16 (v)
  (declare #.*opt-settings* (double-float v))
  (cond ((>= v 1d0) 65535)
        ((<= v 0d0) 0)
        (t (floor (* 65535d0 v)))))


(declaim (inline -png-vals))
(defun -png-vals (indfx vals x y g bitfx)
  (declare #.*opt-settings*
           (function indfx bitfx)
           (type (simple-array double-float) vals)
           (fixnum x y) (double-float g))
  (let* ((ind (funcall indfx x y))
         (a (aref vals (+ 3 ind))))
    (declare (double-float a) (pos-int ind))
    (if (> a 0d0)
      (values (funcall bitfx (-scale-convert (aref vals ind) :s a :gamma g))
              (funcall bitfx (-scale-convert (aref vals (1+ ind)) :s a :gamma g))
              (funcall bitfx (-scale-convert (aref vals (+ ind 2)) :s a :gamma g))
              (funcall bitfx (-scale-convert a :gamma g)))
      (values 0 0 0 0))))


(defun clear (sand &optional c)
  (declare (sandpaint sand))
  (pigment:with ((if c c (sandpaint-bg sand)) r g b a)
    (-do-op (sand size vals indfx)
      (-square-loop (x y size)
        (let ((ind (funcall indfx x y)))
          (declare (fixnum ind))
          (setf (aref vals ind) r
                (aref vals (1+ ind)) g
                (aref vals (+ ind 2)) b
                (aref vals (+ ind 3)) a))))))


(defun clear-fx (sand &key fx)
  (declare (sandpaint sand) (function fx))
  (-do-op (sand size vals indfx)
    (-square-loop (x y size)
      (let ((ind (funcall indfx x y)))
        (declare (fixnum ind))
        (pigment:with ((funcall fx
                         (vec:vec (coerce x 'double-float)
                                  (coerce y 'double-float))) r g b a)
          (setf (aref vals ind) r
                (aref vals (1+ ind)) g
                (aref vals (+ ind 2)) b
                (aref vals (+ ind 3)) a))))))


(defun make (&key (size 1000)
                  (fg (pigment:rgb 0.0d0 0.0d0 0.0d0))
                  (bg (pigment:rgb 1.0d0 1.0d0 1.0d0)))
  (pigment:with (bg r g b a)
    (let ((vals (make-rgba-array size))
          (indfx (-get-indfx size)))
      (declare (function indfx))
      (-square-loop (x y size)
        (let ((ind (funcall indfx x y)))
          (declare (fixnum ind))
          (setf (aref vals ind) r
                (aref vals (1+ ind)) g
                (aref vals (+ ind 2)) b
                (aref vals (+ ind 3)) a)))
      (make-sandpaint :size size :fg fg :bg bg :vals vals :indfx indfx))))


(defun set-fg (sand c)
  (declare (pigment:rgba c))
  (setf (sandpaint-fg sand) c))


(defun set-bg (sand c)
  (declare (pigment:rgba c))
  (setf (sandpaint-bg sand) c))


(defun pix (sand vv)
  (declare (list vv))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for v of-type vec:vec in vv
          do (-inside-floor (size v x y)
               (-operator-over indfx vals x y fg)))))


(defun pix-overlap (sand pts)
  (declare #.*opt-settings* (sandpaint sand) (list pts))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for pt of-type vec:vec in pts
          do (-pix-overlap indfx vals size pt fg))))


(defun pix-overlap* (sand pt)
  (declare #.*opt-settings* (sandpaint sand) (vec:vec pt))
  (-do-op (sand size vals indfx)
    (-pix-overlap indfx vals size pt (sandpaint-fg sand))))


(defun circ (sand vv rad n)
  (declare #.*opt-settings* (list vv) (double-float rad) (fixnum n))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for v of-type vec:vec in vv
          do (-draw-circ indfx vals size v rad n fg))))


(defun bzspl-stroke (sand bz n)
  (declare #.*opt-settings* (fixnum n))
  (-do-op (sand size vals indfx)
    (let ((fg (sandpaint-fg sand)))
      (bzspl:with-rndpos (bz n v)
        (-inside-floor (size v x y)
          (-operator-over indfx vals x y fg))))))


(defun strokes (sand lines grains)
  (declare #.*opt-settings* (fixnum grains) (list lines))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for (u v) in lines
          do (-draw-stroke indfx vals size grains fg u v))))


(defun stroke (sand line grains &key overlap)
  (declare #.*opt-settings* (fixnum grains) (list line))
  (-do-op (sand size vals indfx)
    (if overlap
        (apply #'-draw-stroke-overlap indfx vals size grains (sandpaint-fg sand) line)
        (apply #'-draw-stroke indfx vals size grains (sandpaint-fg sand) line))))


(defun dens-stroke (sand line &optional (dens 1d0))
  (declare #.*opt-settings* (double-float dens) (list line))
  (-do-op (sand size vals indfx)
    (apply #'-draw-dens-stroke indfx vals size dens (sandpaint-fg sand) line)))


(defun lin-path (sand path rad grains &key (dens 1d0))
  (declare #.*opt-settings* (double-float rad dens) (fixnum grains))
  (-do-op (sand size vals indfx)
    (loop with fg = (sandpaint-fg sand)
          for u of-type vec:vec in path
          and w of-type vec:vec in (cdr path)
          do (math:with-linspace ((ceiling (* (vec:dst u w) dens)) 0d0 1d0 p
                                  :end nil)
               (-draw-circ indfx vals size (vec:on-line p u w) rad grains fg)))))


(defun -save8 (sand fn &key gamma)
  "
  save as 8 bits. supports alpha.
  "
  (-do-op (sand size vals indfx)
    (let ((png (make-instance 'zpng::pixel-streamed-png
                              :color-type :truecolor-alpha
                              :width size
                              :height size)))
      (with-open-file
        (fstream (ensure-filename fn ".png") :direction :output
                                             :if-exists :supersede
                                             :if-does-not-exist :create
                                             :element-type '(unsigned-byte 8))
        (declare (stream fstream))
        (zpng:start-png png fstream)
        (-square-loop (x y size)
          (multiple-value-bind (r g b a) (-png-vals indfx vals x y gamma #'-u8)
            (declare (fixnum r g b))
            (zpng:write-pixel (list r g b a) png)))
        (zpng:finish-png png)))))

(defun -save16 (sand fn &key gamma)
  "
  save as 16 bits. does not support alpha.
  "
  (-do-op (sand size vals indfx)
    (let ((img (png:make-image size size 3 16)))
      (-square-loop (x y size)
        (multiple-value-bind (r g b) (-png-vals indfx vals x y gamma #'-u16)
          (declare (fixnum r g b))
          (setf (aref img y x 0) r
                (aref img y x 1) g
                (aref img y x 2) b)))
      (with-open-file (output (ensure-filename fn ".png")
                              :element-type '(unsigned-byte 16)
                              :direction :output :if-exists :supersede)
        (png:encode img output)))))


(defun save (sand fn &key (gamma 1d0) (bits 8))
  (declare (sandpaint sand) (fixnum bits) (double-float gamma))
  (case bits (8 (-save8 sand fn :gamma gamma))
             (16 (-save16 sand fn :gamma gamma))
             (otherwise (error "bits must be 8 or 16. default is 8."))))


(declaim (inline -init-rgb-from-png))
(defun -init-rgb-from-png (indfx vals img s)
  (loop for i from 0 below s do
    (loop for j from 0 below s do
          (setf (aref vals (funcall indfx j i 0)) (-ui8 (aref img i j 0))
                (aref vals (funcall indfx j i 1)) (-ui8 (aref img i j 1))
                (aref vals (funcall indfx j i 2)) (-ui8 (aref img i j 2))
                (aref vals (funcall indfx j i 3)) 1d0))))

; TODO: indexed/grayscale channel only
(defun png-open (fn)
  "read a png image."
  (let ((img (with-open-file (input fn :element-type '(unsigned-byte 8))
               (png:decode input))))
    (destructuring-bind (h w c) (array-dimensions img)
      (declare (ignore c))
      (when (not (= h w)) (error "can only load square images"))
      (let ((sand (make :size h)))
        (-init-rgb-from-png (sandpaint-indfx sand) (sandpaint-vals sand) img h)
        sand))))

