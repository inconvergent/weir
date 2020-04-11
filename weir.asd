;;; use this to hide compiler notes:
;;;
;;; (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

;;; required: (load "~/quicklisp/setup.lisp")

(asdf:defsystem #:weir
  :description "A System for Making Generative Systems"
  :version "3.73.1"
  :author "anders hoff/inconvergent"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:weir/tests)))
  :pathname "src/"
  :serial t
  :depends-on (#:zpng
               #:cl-svg
               #:png
               #:alexandria
               #:cl-json
               #:lparallel
               #:inferior-shell)
  :components ((:file "packages")
               (:file "config")
               (:file "various")
               (:file "fn")
               (:file "state")
               (:file "math/math")
               (:file "rnd/rnd")
               (:file "vec/base")
               (:file "vec/vec")
               (:file "vec/3vec")
               (:file "vec/avec")
               (:file "parallel/main")
               (:file "math/curvature")
               (:file "project/perspective")
               (:file "project/ortho")
               (:file "project/raytrace")
               (:file "draw/cpath")
               (:file "math/path")
               (:file "math/simplify-path")
               (:file "draw/hatch")
               (:file "draw/line-remove")
               (:file "pigment/pigment")
               (:file "pigment/non-alpha")
               (:file "pigment/extra")
               (:file "hset")
               (:file "rnd/extra")
               (:file "rnd/walkers")
               (:file "rnd/3rnd")
               (:file "graph/main")
               (:file "graph/paths")
               (:file "graph/edge-set")
               (:file "graph/mst-cycle")
               (:file "aux/dat")
               (:file "gridfont/main")
               (:file "draw/bzspl")
               (:file "draw/lin-path")
               (:file "draw/sandpaint")
               (:file "draw/sandpaint-extra")
               (:file "draw/draw-svg")
               (:file "distance/zonemap")
               (:file "distance/kdtree")
               (:file "distance/bvh-utils")
               (:file "mesh/mesh")
               (:file "mesh/vert-utils")
               (:file "mesh/shapes")
               (:file "mesh/lights")
               (:file "mesh/obj")
               (:file "mesh/bvh")
               (:file "mesh/line")
               (:file "mesh/raytracer")
               (:file "mesh/raytracer-refract")
               (:file "point-cloud/main")
               (:file "point-cloud/bvh")
               (:file "point-cloud/raytracer")
               (:file "weir/weir")
               (:file "weir/vert-utils")
               (:file "weir/planar-cycles")
               (:file "weir/cycles")
               (:file "weir/3vert-utils")
               (:file "weir/alterations")
               (:file "weir/3alterations")))

(asdf:defsystem #:weir/tests
  :depends-on (#:weir)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':weir-tests
                                           '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "test")
               (:file "bzspl")
               (:file "curvature")
               (:file "graph")
               (:file "hset")
               (:file "kdtree")
               (:file "linear-path")
               (:file "math")
               (:file "mesh")
               (:file "ortho")
               (:file "parallel")
               (:file "pigment")
               (:file "pix-overlap")
               (:file "plot-cpath")
               (:file "plot-outline-path")
               (:file "plot-simplify")
               (:file "plot")
               (:file "point-cloud")
               (:file "rnd")
               (:file "sandpaint")
               (:file "vec")
               (:file "weir-loop")
               (:file "weir")
               (:file "weir3")))
