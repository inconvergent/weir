
(asdf:defsystem #:weir
  :description "A System for Making Generative Systems"
  :version "4.9.1"
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
               (:file "hset")
               (:file "math/math")
               (:file "rnd/rnd")
               (:file "vec/base")
               (:file "vec/vec")
               (:file "vec/checks")
               (:file "vec/3vec")
               (:file "vec/avec")
               (:file "parallel/main")
               (:file "math/curvature")
               (:file "project/perspective")
               (:file "project/ortho")
               (:file "draw/cpath")
               (:file "draw/jpath")
               (:file "math/path")
               (:file "math/simplify-path")
               (:file "draw/hatch")
               (:file "draw/line-remove")
               (:file "pigment/pigment")
               (:file "pigment/non-alpha")
               (:file "pigment/extra")
               (:file "rnd/extra")
               (:file "rnd/3rnd")
               (:file "rnd/walkers")
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
               (:file "weir/props")
               (:file "weir/weir-with-macro")
               (:file "weir/vert-utils")
               (:file "weir/planar-cycles")
               (:file "weir/paths")
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
               (:file "parallel")
               (:file "math")
               (:file "hset")
               (:file "graph")
               (:file "rnd")
               (:file "vec")
               (:file "bzspl")
               (:file "kdtree")
               (:file "linear-path")
               (:file "curvature")
               (:file "ortho")
               (:file "plot")
               (:file "plot-paths")
               (:file "plot-simplify")
               (:file "plot-cpath")
               (:file "plot-jpath")
               (:file "plot-outline-path")
               (:file "pix-overlap")
               (:file "pigment")
               (:file "sandpaint")
               (:file "weir")
               (:file "weir-loop")
               (:file "weir3")))

