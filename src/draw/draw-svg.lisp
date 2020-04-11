
(in-package :draw-svg)

(defparameter *short* 1000d0)
(defparameter *long* 1414.285d0)
(defparameter *svg* 'cl-svg:svg-1.1-toplevel)


(defun -coerce-hex (c)
  (if (equal (type-of c) 'pigment:rgba) (pigment:to-hex c) c))


(defstruct draw-svg
  (layout nil :type symbol :read-only t)
  (width 0d0 :type double-float :read-only t)
  (height 0d0 :type double-float :read-only t)
  (stroke "black" :type string :read-only nil)
  (stroke-width 1.1d0 :type double-float :read-only nil)
  (rep-scale 1d0 :type double-float :read-only nil)
  (scene nil :read-only nil))


(defun -view-box (width height)
  (format nil "0 0 ~f ~f" width height))

(defun -get-scene (layout)
  (case layout
    (:a4-landscape (cl-svg:make-svg-toplevel *svg* :height "210mm" :width "297mm"
                     :view-box (-view-box *long* *short*)))
    (:a4-portrait (cl-svg:make-svg-toplevel *svg* :height "297mm" :width "210mm"
                    :view-box (-view-box *short* *long*)))
    (:a3-landscape (cl-svg:make-svg-toplevel *svg* :height "297mm" :width "420mm"
                     :view-box (-view-box *long* *short*)))
    (:a3-portrait (cl-svg:make-svg-toplevel *svg* :height "420mm" :width "297mm"
                    :view-box (-view-box *short* *long*)))
    (otherwise (error "invalid layout. use: :a4-portrait, :a4-landscape,
      :a3-landscape or :a3-portrait; or use (make* :height h :width w)"))))


(defun -get-width-height (layout)
  (case layout (:a4-landscape (list *long* *short*))
               (:a4-portrait (list *short* *long*))
               (:a3-landscape (list *long* *short*))
               (:a3-portrait (list *short* *long*))))

(defun make (&key (layout :a4-landscape) stroke
                  (stroke-width 1.1d0) (rep-scale 1d0))
  (destructuring-bind (width height) (-get-width-height layout)
    (make-draw-svg :layout layout
                   :stroke-width stroke-width
                   :stroke (-coerce-hex (if stroke stroke "black"))
                   :rep-scale rep-scale
                   :height height :width width
                   :scene (-get-scene layout))))


(defun make* (&key (height 1000d0) (width 1000d0) stroke
                   (stroke-width 1.1d0) (rep-scale 1d0))
  (make-draw-svg :layout 'custom
                 :stroke-width stroke-width
                 :rep-scale rep-scale
                 :stroke (-coerce-hex (if stroke stroke "black"))
                 :height height :width width
                 :scene (cl-svg:make-svg-toplevel *svg*
                          :height height :width width)))


(defun set-stroke (psvg stroke)
  (declare (draw-svg psvg))
  (setf (draw-svg-stroke psvg) (-coerce-hex stroke)))


(defun set-stroke-width (psvg sw)
  (declare (draw-svg psvg) (double-float sw))
  (setf (draw-svg-stroke-width psvg) sw))


(defun set-rep-scale (psvg rs)
  (declare (draw-svg psvg) (double-float rs))
  (setf (draw-svg-stroke-width psvg) rs))


(defun get-rnd-svg-color ()
  (rnd:rndget (list "gray" "black" "red" "maroon" "yellow" "olive" "lime"
                    "green" "aqua" "teal" "blue" "navy" "fuchsia" "purple")))


(defun -move-to (p)
  (declare (vec:vec p))
  (cl-svg:move-to (vec:vec-x p) (vec:vec-y p)))

(defun -line-to (p)
  (declare (vec:vec p))
  (cl-svg:line-to (vec:vec-x p) (vec:vec-y p)))

(defun -quadratic (p q)
  (declare (vec:vec p q))
  (format nil "Q~f,~f ~f,~f" (vec:vec-x p) (vec:vec-y p)
                             (vec:vec-x q) (vec:vec-y q)))

(defun -arccirc (xy r &aux (r2 (* 2d0 r)))
  (format nil "M~f,~f m -~f,0 a ~f,~f 0 1,0 ~f 0 a ~f,~f 0 1,0 -~f 0"
              (vec:vec-x xy) (vec:vec-y xy) r r r r2 r r r2))

;https://stackoverflow.com/questions/5736398/how-to-calculate-the-svg-path-for-an-arc-of-a-circle
(defun -carc (xy rad a b)
  (let ((axy (vec:from xy (vec:cos-negsin a) rad))
        (bxy (vec:from xy (vec:cos-negsin b) rad))
        (arcflag (if (< (- b a) PI) 0 1)))
   (format nil "M ~f,~f A ~f,~f 0 ~d,0 ~f ~f"
     (vec:vec-x axy) (vec:vec-y axy) rad rad arcflag
     (vec:vec-x bxy) (vec:vec-y bxy))))


(defun -select-stroke (psvg stroke)
  (if stroke stroke (draw-svg-stroke psvg)))


(defun -select-rep-scale (psvg rs)
  (if rs rs (draw-svg-rep-scale psvg)))


(defun -select-fill (fill)
  (if fill fill "none"))


(defun -accumulate-path (pth a)
  (cl-svg:with-path pth
    (if (> (length pth) 0) (-line-to a) (-move-to a))))


(defun compound (psvg components &key sw fill stroke (fo 1d0) (so 1d0))
  (declare (draw-svg psvg) (sequence components))

  (with-struct (draw-svg- scene stroke-width) psvg
    (cl-svg:draw scene
      (:path
        :d (loop with pth = (cl-svg:make-path)
                 for (ct c) in components
                 do (case ct (:path (loop for p in c
                                          do (-accumulate-path pth p)))
                             (:bzspl (list)))
                 finally (return pth)))
      :fill (-select-fill fill)
      :fill-opacity fo
      :stroke-opacity so
      :stroke (-select-stroke psvg stroke)
      :stroke-width (if sw sw stroke-width))))


(defun path (psvg pts &key sw fill stroke (fo 1d0) (so 1d0) closed)
  (declare (draw-svg psvg) (list pts))
  (with-struct (draw-svg- scene stroke-width) psvg
    (cl-svg:draw scene
      (:path :d (loop with pth = (cl-svg:make-path)
                        for p of-type vec:vec in pts
                        do (-accumulate-path pth p)
                        finally (when closed (cl-svg:with-path pth "Z"))
                                (return pth)))
      :fill (-select-fill fill)
      :fill-opacity fo
      :stroke-opacity so
      :stroke (-select-stroke psvg stroke)
      :stroke-width (if sw sw stroke-width))))


(defun show-boundary (psvg &key sw (stroke "red"))
  (declare (draw-svg psvg))
  (with-struct (draw-svg- width height) psvg
    (let ((mw (* 0.5d0 width))
          (mh (* 0.5d0 height)))
      (path psvg (vec:rect mw mh :xy (vec:vec mw mh)) :closed t
                                 :sw sw :stroke stroke))))


(defun show-crop (psvg &key (len 3d0) sw (stroke "blue"))
  (declare (draw-svg psvg))
  (with-struct (draw-svg- width height) psvg
    (loop for m in (list (list (vec:vec 0d0 0d0) (vec:vec len 0d0))
                         (list (vec:vec width 0d0) (vec:vec (- width len) 0d0))
                         (list (vec:vec 0d0 height) (vec:vec len height))
                         (list (vec:vec width height)
                               (vec:vec (- width len) height)))
          do (path psvg m :closed t :sw sw :stroke stroke))))


(defun rect (psvg w h &key fill (xy vec:*zero*) (fo 1d0) (so 1d0))
  (declare (draw-svg psvg) (double-float w h))
  (path psvg (vec:rect w h :xy xy) :closed t :fill (-select-fill fill)
        :fo fo :so so))

(defun square (psvg s &key fill (xy vec:*zero*) (fo 1d0) (so 1d0))
  (declare (draw-svg psvg) (double-float s))
  (rect psvg s s :fill fill :xy xy :fo fo :so so))


; ----- HATCH -----

(defun -get-pts (pts closed)
  (declare (sequence pts))
  (let ((res (make-adjustable-vector))
        (is-cons (equal (type-of pts) 'cons)))
    (declare (vector res))
    (if is-cons (loop for p of-type vec:vec in pts do (vextend p res))
                (loop for p of-type vec:vec across pts do (vextend p res)))
    (when closed (vextend (if is-cons (first pts) (aref pts 0)) res))
    res))

(defun hatch (psvg pts
              &key (angles (list 0d0 (* 0.5d0 PI)))
                   stitch drop closed rs sw (so 1d0) stroke
              &aux (stroke* (-select-stroke psvg stroke))
                   (draw (if drop
                             (lambda (p) (rnd:prob drop nil
                                           (draw-svg:path psvg p :sw sw :so so
                                             :stroke stroke*)))
                             (lambda (p) (draw-svg:path psvg p :sw sw :so so
                                           :stroke stroke*)))))
  (declare (function draw))
  (let ((res (hatch:hatch (-get-pts pts closed)
               :angles angles :rs (-select-rep-scale psvg rs))))
    (loop for h across (if stitch (hatch:stitch res) res) do
      (if (and (> (length h) 0) (every #'identity h))
        (funcall draw h)))))


; ----- BZSPL -----

(defun -fl (a) (declare (list a)) (first (last a)))

(defun -bzspl-do-open (pts pth)
  (cl-svg:with-path pth (-move-to (first pts)))
  (if (= (length pts) 3)
    ; 3 pts
    (cl-svg:with-path pth (-quadratic (second pts) (third pts)))
    ; more than 3 pts
    (let ((inner (subseq pts 1 (1- (length pts)))))
      (loop for a in inner
            and b in (cdr inner)
            do (cl-svg:with-path pth (-quadratic a (vec:mid a b))))
      (cl-svg:with-path pth (-quadratic (-fl inner) (-fl pts))))))


(defun -roll-once (a)
  (declare (list a))
  (append (subseq a 1) (list (first a))))

(defun -bzspl-do-closed (pts pth)
  (cl-svg:with-path pth (-move-to (vec:mid (-fl pts) (first pts))))
  (loop for a in pts
        and b in (-roll-once pts)
        do (cl-svg:with-path pth (-quadratic a (vec:mid a b)))))


(defun bzspl (psvg pts &key closed sw stroke fill (so 1d0) (fo 1d0))
  "
  quadratic bezier
  "
  (declare (draw-svg psvg))
  (when (< (length pts) 3) (error "needs at least 3 pts."))

  (with-struct (draw-svg- scene stroke-width) psvg
    (let ((pth (cl-svg:make-path)))
      (if closed (-bzspl-do-closed pts pth)
                 (-bzspl-do-open pts pth))
      (cl-svg:draw scene (:path :d (cl-svg:path pth))
                   :stroke (-select-stroke psvg stroke)
                   :fill (-select-fill fill)
                   :stroke-opacity so
                   :fill-opacity fo
                   :stroke-width (if sw sw stroke-width)))))


; ----- WPATH -----

(defun wpath (psvg pts &key width sw rs (opposite t) (cap t) stroke (so 1d0) ns)
  (declare (draw-svg psvg) (list pts) (boolean opposite cap) (double-float so))

  (when (and ns rs) (error "either rs or ns must be nil"))
  (with-struct (draw-svg- scene stroke-width) psvg
    (when (or (not width)
              (< #1=(if ns ns (ceiling (* (-select-rep-scale psvg rs) width))) 2))
      ; single stroke
      (return-from wpath
        (path psvg pts :sw sw :stroke stroke :so so)))
    ; wide path
    (let ((pth (cl-svg:make-path))
          (rep #1#)
          (rup (* width 0.5d0))
          (rdown (* width -0.5d0)))

      (when (and opposite (= 0 (math:mod2 rep))) (incf rep))

      (loop for a in pts
            and b in (cdr pts)
            do (when cap (-accumulate-path pth a))
               (loop with pab = (vec:norm (vec:perp (vec:sub b a)))
                     for s in (math:linspace rep rdown rup)
                     and i from 0
                     do (-accumulate-path pth
                          (vec:from (if #2=(= (math:mod2 i) 0) a b) pab s))
                        (-accumulate-path pth
                          (vec:from (if #2# b a) pab s)))
               (when cap (-accumulate-path pth (if (= (math:mod2 rep) 0) a b))))

      (cl-svg:draw scene
                   (:path :d (cl-svg:path pth))
                   :stroke (-select-stroke psvg stroke)
                   :fill "none"
                   :stroke-opacity so
                   :stroke-width (if sw sw stroke-width)))))


; ----- CPATH -----

(defun cpath (psvg pts &key (width 1d0) closed (clim -0.5d0) stroke
                            (slim -0.95d0) sw rs (so 1d0) ns)
  (declare (draw-svg psvg) (list pts) (double-float so clim slim))
  (when (and ns rs) (error "either rs or ns must be nil"))
  (let ((rep (if ns ns (ceiling (* (-select-rep-scale psvg rs) width)))))
    (when (< rep 2)
      (return-from cpath (path psvg pts :stroke stroke
                               :sw sw :so so :closed closed)))
    (path psvg (cpath:cpath (to-vector (if closed (math:close-path pts) pts))
                            (* width 0.5d0) rep
                            :closed closed :slim slim :clim clim)
          :stroke stroke :sw sw :so so)))


; ----- CARC -----

(defun carc (psvg xy rad a b &key fill sw stroke (so 1d0))
  "
  arc between angles (a b) centered at xy. rotation is ccw
  "
  (declare (draw-svg psvg) (vec:vec xy) (double-float rad a b))
  (with-struct (draw-svg- scene stroke-width) psvg
    (cl-svg:draw scene (:path :d (-carc xy rad a b))
                 :fill (-select-fill fill)
                 :stroke (-select-stroke psvg stroke)
                 :stroke-width (if sw sw stroke-width)
                 :stroke-opacity so)))


; ----- CIRC -----

(defun circ (psvg xy rad &key fill sw aspath stroke (so 1d0) (fo 1d0))
  (declare (draw-svg psvg) (vec:vec xy) (double-float rad))
  (with-struct (draw-svg- scene stroke-width) psvg
    (let ((sw* (if sw sw stroke-width)))
      (if aspath
        (cl-svg:draw scene (:path :d (-arccirc xy rad))
                     :fill (-select-fill fill)
                     :stroke (-select-stroke psvg stroke)
                     :stroke-width sw*
                     :fill-opacity fo :stroke-opacity so)
        (cl-svg:draw scene (:circle :cx (vec:vec-x xy) :cy (vec:vec-y xy) :r rad)
                     :fill (-select-fill fill)
                     :stroke (-select-stroke psvg stroke)
                     :stroke-width sw*
                     :fill-opacity fo :stroke-opacity so)))))


; TODO: fxn to do this with multiple rads?
(defun circs (psvg vv rad &key fill stroke sw aspath (fo 1d0) (so 1d0))
  (declare (draw-svg psvg) (list vv) (double-float rad))
  (loop for xy of-type vec:vec in vv
        do (circ psvg xy rad :fill fill :sw sw :stroke stroke
                 :aspath aspath :fo fo :so so)))


; ----- WCIRC -----

(defun wcirc (psvg xy rad &key outer-rad sw rs stroke (so 1d0))
  (declare (draw-svg psvg))
  (let* ((inner-rad (max 0.1d0 (if outer-rad rad 0.1d0)))
         (outer-rad* (if outer-rad outer-rad rad))
         (n (ceiling (* (abs (- outer-rad* inner-rad))
                        (-select-rep-scale psvg rs)))))
  (with-struct (draw-svg- scene stroke-width) psvg
    (cl-svg:draw scene
      (:path :d (loop with pth = (cl-svg:make-path)
                      for r of-type double-float in
                        (math:linspace n inner-rad outer-rad*)
                      do (cl-svg:with-path pth (-arccirc xy r))
                      finally (return pth)))
      :fill "none"
      :stroke (-select-stroke psvg stroke)
      :stroke-width (if sw sw stroke-width)
      :stroke-opacity so))))


(defun draw (psvg d &key sw stroke fill (so 1d0) (fo 1d0))
  (declare (draw-svg psvg) (vector d))
  (with-struct (draw-svg- scene stroke-width) psvg
    (cl-svg:draw scene (:path :d d)
                 :fill (-select-fill fill)
                 :stroke (-select-stroke psvg stroke)
                 :stroke-width (if sw sw stroke-width)
                 :stroke-opacity so
                 :fill-opacity fo)))


(defun save (psvg fn)
  (declare (draw-svg psvg))
  (with-struct (draw-svg- scene) psvg
    (with-open-file (fstream (ensure-filename fn ".svg")
                       :direction :output :if-exists :supersede)
      (declare (stream fstream))
      (cl-svg:stream-out fstream scene))))

