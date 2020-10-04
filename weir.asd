
(asdf:defsystem #:weir
  :description "A System for Making Generative Systems"
  :version "4.0.3"
  :author "anders hoff/inconvergent"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:weir/tests)))
  :pathname "src/"
  :serial t
  :depends-on (#:alexandria
               #:cl-json
               #:cl-svg
               #:inferior-shell
               #:lparallel
               #:png
               #:split-sequence
               #:zpng)
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
               (:file "auxiliary/dat")
               (:file "auxiliary/obj")
               (:file "gridfont/main")
               (:file "draw/bzspl")
               (:file "draw/lin-path")
               (:file "draw/sandpaint")
               (:file "draw/sandpaint-extra")
               (:file "draw/draw-svg")
               (:file "distance/zonemap")
               (:file "distance/kdtree")
               (:file "weir/weir")
               (:file "weir/weir-macro")
               (:file "weir/weir-with-macro")
               (:file "weir/vert-utils")
               (:file "weir/planar-cycles")
               (:file "weir/cycles")
               (:file "weir/3vert-utils")
               (:file "weir/alterations")
               (:file "weir/3alterations")
               (:file "weir/weir-extra")))

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
               (:file "ortho")
               (:file "parallel")
               (:file "pigment")
               (:file "pix-overlap")
               (:file "plot-cpath")
               (:file "plot-outline-path")
               (:file "plot-simplify")
               (:file "plot")
               (:file "rnd")
               (:file "sandpaint")
               (:file "vec")
               (:file "weir")
               (:file "weir-loop")
               (:file "weir3")))

