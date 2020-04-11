#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")

(parallel:init :cores 8)


; makes (omni) light sources
(defun make-lights (rad &key (alpha 1d0) (sat 0.8d0) (val 0.85d0))
  (let ((cyan (pigment:hsv (/ 196d0 360d0) sat val alpha))
        (black (pigment:black alpha))
        (white (pigment:white alpha))
        (magenta (pigment:hsv (/ 281d0 360d0) sat val alpha))
        (orange (pigment:hsv (/ 38d0 360d0) sat val alpha))
        (blood (pigment:hsv (/ 326d0 360d0) sat val alpha)))
    (math:nrep 3
      (list (rnd:rcond (0.2d0 cyan)
                       (0.2d0 magenta)
                       (0.2d0 blood)
                       (0.2d0 orange))
            (rnd:3on-sphere :rad rad)))))

; naive (and slow) recursive raytracer with reflections
(defun make-renderer (raycast lights &key (depth 10) (rk 0.4d0)
                                     &aux (invrk (- 1d0 rk)))
  (labels
    ((do-diffuse (poly n p)
      (declare (list poly) (vec:3vec n p))
      (loop with res = (pigment:black)
            for (color pos) in lights
            do (let* ((dir (vec:3norm! (vec:3sub pos p)))
                      (dot (vec:3dot n dir)))
                 (when (and (< 0d0 dot)
                            (not (funcall raycast
                                   (list (vec:3from p dir 0.0001d0) pos))))
                       (pigment:safe-clamp!
                         (pigment:non-a-scale-add! res color dot))))
            finally (return res)))

     (flip-normal (n dir)
       (declare (vec:3vec n dir))
       (if (< (vec:3dot dir n) 0d0) n (vec:3neg n)))

     (line (p dir)
       (declare (vec:3vec p dir))
       (list (vec:3from p dir 0.0001d0)
             (vec:3from p dir 10000d0)))

     (shade (poly n* p ray &key d)
       (declare (list poly) (vec:3vec n* p) (list ray))
       (let* ((dir (vec:3norm! (apply #'vec:3isub ray)))
              (n (flip-normal n* dir))
              (c (pigment:non-a-scale! (do-diffuse poly n p) invrk)))
         (declare (vec:3vec dir n) (pigment:rgba c))
         (when (< 0 d)
               (pigment:safe-clamp!
                 (pigment:non-a-scale-add! c
                   (raytrace (line p (vec:3norm! (vec:3norm-reflect dir n)))
                             :d (1- d))
                 rk)))
         c))

     (raytrace (ray &key (d depth))
       (let ((h (funcall raycast ray)))
         (if h (destructuring-bind (_ p poly normal) h
                 (shade poly normal p ray :d d))
               (pigment:white)))))

    #'raytrace))

(defun main (size fn)
  (let* ((black (pigment:black))
         (white (pigment:white))
         (cam (vec:3rep 3d0))
         (proj (ortho:make :cam cam
                           :look (vec:3rep 0d0)
                           :s 200d0
                           :xy (vec:rep 500d0)))
         (msh (mesh:obj-load (mesh:make) "./teapot.obj"))
         (lights (make-lights 2000d0))
         (sand (sandpaint:make :size size :bg (pigment:gray 0.14d0)
                                          :fg (pigment:white 0.8d0))))

    (mesh:center! msh)

    (let* ((vertfx (mesh:make-vert-getter msh)) ; vertex accessor
           (bvh (mesh:make-bvh msh :vertfx vertfx :num 3)) ; bvh structure
           (raycast (mesh:make-bvh-raycaster bvh))
           (render (make-renderer raycast lights))
           (aa 10) ; samples per pixel. higher is slower
           (raa (/ 1d0 (coerce aa 'double-float)))
           (disc-ray (ortho::make-sample-disc-ray proj ; depth of field offset
                       :rad 0.3d0 :d (* 0.9d0 (vec:3len cam)))))

      (ortho:parallel-pixel-render proj size
        (lambda (i j pt) (declare (fixnum i j) (vec:3vec pt))
          (loop with c of-type pigment:rgba = (pigment:black)
                repeat aa
                do (pigment:non-a-add! c
                     (funcall (the function render)
                              (funcall (the function disc-ray) pt)))
                finally (sandpaint::set-pix sand i j
                          (pigment:non-a-scale! c raa))))
        :parts 50))

    (sandpaint:save sand fn)))


(time (main 1000 (second (cmd-args))))

