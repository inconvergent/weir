(defpackage #:weir-utils
  (:use #:common-lisp)
  (:export
   #:*opt-settings*
   #:PI5
   #:PII
   #:abbrev
   #:aif
   #:append-number
   #:append-postfix
   #:cmd-args
   #:define-struct-load-form
   #:ensure-filename
   #:ensure-vector
   #:internal-path-string
   #:lvextend
   #:mac
   #:make-adjustable-vector
   #:mkstr
   #:numshow
   #:print-every
   #:psymb
   #:string-list-concat
   #:to-adjustable-vector
   #:to-list
   #:to-vector
   #:undup
   #:vector-first
   #:vector-last
   #:vextend
   #:with-struct))

(defpackage #:fn
  (:use #:common-lisp)
  (:export
    #:fn))

(defpackage #:vec
  (:use #:common-lisp)
  (:export
    #:*3one*
    #:*3zero*
    #:*one*
    #:*zero*
    #:3add
    #:3add!
    #:3copy
    #:3cross
    #:3cross!
    #:3div
    #:3div!
    #:3dot
    #:3dst
    #:3dst*
    #:3dst2
    #:3from
    #:3from!
    #:3from-vec
    #:3idiv
    #:3idiv!
    #:3isub
    #:3isub!
    #:3ladd
    #:3ladd!
    #:3ladd!*
    #:3ladd*
    #:3ldiv
    #:3ldiv!
    #:3ldiv!*
    #:3ldiv*
    #:3ldot
    #:3ldst
    #:3ldst*
    #:3len
    #:3len2
    #:3lfrom-vec
    #:3lidiv!
    #:3lidiv!*
    #:3lisub
    #:3lisub!
    #:3lisub!*
    #:3lisub*
    #:3lmid
    #:3lmult
    #:3lmult!
    #:3lmult!*
    #:3lmult*
    #:3lon-line
    #:3lon-line*
    #:3lrot*
    #:3lsmult!*
    #:3lsmult*
    #:3lsub
    #:3lsub!
    #:3lsub!*
    #:3lsub*
    #:3lsum
    #:3lto-vec
    #:3maxnrm
    #:3mid
    #:3mult
    #:3mult!
    #:3neg
    #:3norm
    #:3norm!
    #:3norm-project
    #:3norm-reflect
    #:3norm-reject
    #:3nsub
    #:3on-line
    #:3planex
    #:3polyx
    #:3refract
    #:3rep
    #:3rot
    #:3sadd
    #:3sadd!
    #:3sdiv
    #:3sdiv!
    #:3set!
    #:3smult
    #:3smult!
    #:3spherex
    #:3ssub
    #:3ssub!
    #:3sub
    #:3sub!
    #:3to-list
    #:3to-vec
    #:3vabs
    #:3vec
    #:3vec*
    #:3vec-x
    #:3vec-y
    #:3vec-z
    #:3with-xy
    #:3zero
    #:add
    #:add!
    #:angle
    #:copy
    #:cos-negsin
    #:cos-sin
    #:cross
    #:div
    #:div!
    #:dot
    #:dst
    #:dst*
    #:dst2
    #:flip
    #:from
    #:from!
    #:idiv
    #:idiv!
    #:isub
    #:isub!
    #:ladd
    #:ladd!
    #:ladd!*
    #:ladd*
    #:ldiv
    #:ldiv!
    #:ldiv!*
    #:ldiv*
    #:ldot
    #:ldst
    #:ldst*
    #:len
    #:len2
    #:lidiv
    #:lidiv!
    #:lidiv!*
    #:lisub
    #:lisub!
    #:lisub!*
    #:lisub*
    #:lmid
    #:lmid
    #:lmult
    #:lmult!
    #:lmult!*
    #:lmult*
    #:lon-line
    #:lon-line*
    #:lrot
    #:lround
    #:lsmult!*
    #:lsmult*
    #:lsub
    #:lsub!
    #:lsub!*
    #:lsub*
    #:lsum
    #:maxnrm
    #:mid
    #:mult
    #:mult!
    #:neg
    #:norm
    #:norm!
    #:norm-project
    #:norm-reflect
    #:norm-reject
    #:nsub
    #:on-circ
    #:on-line
    #:perp
    #:polygon
    #:ptinside
    #:rect
    #:refract
    #:rep
    #:rot
    #:sadd
    #:sadd!
    #:sdiv
    #:sdiv!
    #:segdst
    #:segx
    #:set!
    #:sin-cos
    #:smult
    #:smult!
    #:square
    #:ssub
    #:ssub!
    #:sub
    #:sub!
    #:to-list
    #:vec
    #:vec*
    #:vec-x
    #:vec-y
    #:zero
    #:with-xy)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:vextend
    #:ensure-vector
    #:make-adjustable-vector))

(defpackage #:avec
  (:use #:common-lisp)
  (:export
    #:3avec
    #:3dst
    #:3dst2
    #:3getv
    #:3minmax
    #:3setv
    #:3with-vec
    #:avec
    #:dst
    #:dst2
    #:getv
    #:minmax
    #:with-vec
    #:setv)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:to-list
    #:make-adjustable-vector
    #:vextend))

(defpackage #:parallel
  (:use #:common-lisp)
  (:export
    #:create-channel
    #:end
    #:info
    #:init)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-filename
    #:internal-path-string))

(defpackage #:perspective
  (:use #:common-lisp)
  (:export
    #:make
    #:make*
    #:get-projector
    #:project)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:PII
    #:with-struct))

(defpackage #:ortho
  (:use #:common-lisp)
  (:export
    #:export-data
    #:gauss
    #:import-data
    #:make
    #:make-rayfx
    #:pan-cam
    #:pan-xy
    #:parallel-pixel-render
    #:project
    #:project*
    #:project-offset
    #:project-offset*
    #:rotate
    #:update
    #:zoom)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:PI5
    #:PII
    #:make-adjustable-vector
    #:to-vector
    #:vextend
    #:with-struct))

(defpackage #:math
  (:use #:common-lisp)
  (:export
    #:3cross
    #:3path-length
    #:add
    #:argmax
    #:argmin
    #:clamp
    #:close-path
    #:copy-sort
    #:dadd
    #:ddiv
    #:ddst
    #:dmean
    #:dmod
    #:dmult
    #:dsub
    #:dsum
    #:imod
    #:integer-search
    #:lerp
    #:lget
    #:line-from
    #:linspace
    #:list>than
    #:llerp
    #:lpos
    #:mid-rad
    #:mod2
    #:mult
    #:nrep
    #:path-angles
    #:path-length
    #:path-normals-closed
    #:path-normals-open
    #:path-tangents
    #:percentiles
    #:range
    #:range-search
    #:stipple
    #:sub
    #:sum
    #:with-linspace)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:PI5
    #:ensure-vector
    #:make-adjustable-vector
    #:to-adjustable-vector
    #:to-list
    #:to-vector
    #:vector-last
    #:vextend))

(defpackage #:curvature
  (:use #:common-lisp)
  (:export
    #:ddxy
    #:kappa
    #:offset-paths
    #:offsets)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:PI5
    #:ensure-vector
    #:make-adjustable-vector
    #:vextend))

(defpackage #:rnd
  (:use #:common-lisp)
  (:export
    #:3in-box
    #:3in-cube
    #:3in-sphere
    #:3nin-box
    #:3nin-cube
    #:3non-line
    #:3non-line*
    #:3on-line
    #:3on-line*
    #:3on-sphere
    #:array-split
    #:bernoulli
    #:either
    #:get-acc-circ-walk
    #:get-acc-lin-walk
    #:get-acc-lin-walk*
    #:get-circ-walk
    #:get-lin-walk
    #:get-lin-walk*
    #:in-circ
    #:in-rect
    #:in-square
    #:make-rnd-state
    #:nin-circ
    #:nin-rect
    #:nin-square
    #:non-circ
    #:non-line
    #:non-line*
    #:norm
    #:nrnd
    #:nrnd*
    #:nrnd-from
    #:nrnd-u-from
    #:nrndi
    #:nrndrng
    #:nrndrngi
    #:on-circ
    #:on-line
    #:on-line*
    #:prob
    #:probsel
    #:rcond
    #:rep
    #:reprng
    #:rnd
    #:rnd*
    #:rndget
    #:rndi
    #:rndrng
    #:rndrngi
    #:rndspace
    #:rndspacei
    #:set-rnd-state
    #:shuffle
    #:with-in-circ
    #:with-in-rect
    #:with-on-line
    #:with-prob
    #:with-rndspace)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-vector
    #:make-adjustable-vector
    #:to-vector
    #:vextend))

(defpackage #:state
  (:use #:common-lisp)
  (:export
    #:awith
    #:kset
    #:make
    #:mget
    #:mset
    #:sget
    #:sset
    #:with)
  (:import-from #:weir-utils))

(defpackage #:pigment
  (:use #:common-lisp)
  (:export
    #:as-hsv
    #:black
    #:blood
    #:blue
    #:cmyk
    #:cyan
    #:dark
    #:from-list
    #:gray
    #:green
    #:hsv
    #:magenta
    #:mdark
    #:non-a-add
    #:non-a-add!
    #:non-a-scale
    #:non-a-scale!
    #:non-a-scale-add!
    #:orange
    #:red
    #:rgb
    #:rgba
    #:safe-clamp
    #:safe-clamp!
    #:scale
    #:scale!
    #:to-hex
    #:to-list
    #:to-list*
    #:transparent
    #:vdark
    #:white
    #:with)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-vector))

(defpackage #:hset
  (:use #:common-lisp)
  (:export
    #:add
    #:add*
    #:copy
    #:del
    #:del*
    #:inter
    #:make
    #:mem
    #:mem*
    #:num
    #:symdiff
    #:uni
    #:to-list)
  (:import-from #:weir-utils
    #:*opt-settings*))

(defpackage #:graph
  (:use #:common-lisp)
  (:export
    #:add
    #:copy
    #:cycle->edge-set
    #:cycle-basis->edge-sets
    #:del
    #:edge-set->cycle
    #:edge-set->graph
    #:edge-set-symdiff
    #:edge-sets->cycle-basis
    #:get-continous-paths
    #:get-cycle-basis
    #:get-edges
    #:get-incident-edges
    #:get-min-spanning-tree
    #:get-num-edges
    #:get-num-verts
    #:get-spanning-tree
    #:get-verts
    #:make
    #:mem
    #:del-simple-filaments
    #:vmem
    #:with-graph-edges)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vector-last
    #:vextend
    #:with-struct))

(defpackage #:bzspl
  (:use #:common-lisp)
  (:export
    #:adaptive-pos
    #:len
    #:make
    #:normal
    #:pos
    #:pos*
    #:rndpos
    #:tangent
    #:with-rndpos)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:PI5
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vector-last
    #:vextend
    #:with-struct))

(defpackage #:simplify-path
  (:use #:common-lisp)
  (:export
    #:simplify)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:make-adjustable-vector
    #:vextend))

(defpackage #:lin-path
  (:use #:common-lisp)
  (:export
    #:make
    #:move
    #:pos
    #:pos*
    #:rndpos)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:with-struct))

(defpackage #:hatch
  (:use #:common-lisp)
  (:export
    hatch
    stitch)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-vector
    #:vector-last
    #:make-adjustable-vector
    #:to-list
    #:vextend))

(defpackage #:line-remove
  (:use #:common-lisp)
  (:export
    #:make
    #:stats
    #:path-split)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:PII
    #:ensure-vector
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vector-last
    #:with-struct
    #:vextend))

(defpackage #:cpath
  (:use #:common-lisp)
  (:export
    #:cpath
    #:outline
    #:path-offset
    #:get-diagonals)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-vector
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vextend))

(defpackage #:zonemap
  (:use #:common-lisp)
  (:export
    #:make
    #:verts-in-rad
    #:with-verts-in-rad)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:make-adjustable-vector
    #:vextend
    #:with-struct))

(defpackage #:dat
  (:use #:common-lisp)
  (:export
    #:do-lines-as-buffer
    #:export-data
    #:import-data)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-filename))

(defpackage #:gridfont
  (:use #:common-lisp)
  (:export
    #:make
    #:nl
    #:update
    #:wc)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-filename
    #:internal-path-string
    #:with-struct))

(defpackage #:kdtree
  (:use #:common-lisp)
  (:export
    #:make
    #:make*
    #:rad
    #:nn)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:to-vector
    #:make-adjustable-vector
    #:vextend
    #:with-struct))

(defpackage #:bvh-utils
  (:use #:common-lisp)
  (:export
    #:bvh-node
    #:bvh-node-l
    #:bvh-node-leaves
    #:bvh-node-ma
    #:bvh-node-mi
    #:bvh-node-r
    #:bvh-result
    #:bvh-result-hit
    #:bvh-result-pt
    #:bvh-result-result
    #:bvh-result-s
    #:make-bvh-result
    #:make-line-bbox-test
    #:update-bvh-result)
  (:import-from #:weir-utils
    #:*opt-settings*))

(defpackage #:sandpaint
  (:use #:common-lisp)
  (:export
    #:bzspl-stroke
    #:chromatic-aberration
    #:circ
    #:clear
    #:clear-fx
    #:dens-stroke
    #:get-size
    #:lin-path
    #:make
    #:pix
    #:pix-overlap
    #:pix-overlap*
    #:png-open
    #:sample
    #:save
    #:set-bg
    #:set-fg
    #:stroke
    #:strokes)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:ensure-filename
    #:with-struct))

(defpackage #:draw-svg
  (:use #:common-lisp)
  (:export
    #:*short*
    #:*long*
    #:bzspl
    #:carc
    #:circ
    #:circs
    #:compound
    #:cpath
    #:draw
    #:get-rnd-svg-color
    #:hatch
    #:make
    #:make*
    #:path
    #:rect
    #:save
    #:set-rep-scale
    #:set-stroke
    #:set-stroke-width
    #:show-boundary
    #:show-crop
    #:square
    #:wcirc
    #:wpath)
  (:import-from #:weir-utils
    #:PI
    #:ensure-filename
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vector-last
    #:vextend
    #:with-struct))

(defpackage #:mesh
  (:use #:common-lisp)
  (:export
    #:add-box!
    #:add-face!
    #:add-open-box!
    #:add-polygon!
    #:add-polygons!
    #:add-polyhedra!
    #:add-rect!
    #:add-vert!
    #:add-verts!
    #:center!
    #:del-polygon!
    #:edge-length
    #:extrude-polygon!
    #:get-all-edges
    #:get-all-polygons
    #:get-all-verts
    #:get-edge-polygons
    #:get-num-edges
    #:get-num-polygons
    #:get-num-verts
    #:get-polygon-edges
    #:get-vert
    #:get-verts
    #:make
    #:make-bvh
    #:make-bvh-raycaster
    #:make-cone-light
    #:make-hidden-fx
    #:make-raycaster
    #:make-raytracer
    #:make-refraction-raytracer
    #:make-unhidden-line-fx
    #:make-vert-getter
    #:move-vert!
    #:normal
    #:obj-load
    #:polyx
    #:refract-or-reflect
    #:transform!)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vextend
    #:with-struct))

(defpackage #:point-cloud
  (:use #:common-lisp)
  (:export
    #:get-color
    #:get-num-points
    #:make
    #:make-bvh
    #:make-bvh-raycaster
    #:make-point-getter)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vextend
    #:with-struct))

(defpackage #:weir
  (:use #:common-lisp)
  (:export
    #:3add-box!
    #:3add-cube!
    #:3add-path!
    #:3add-vert!
    #:3add-vert?
    #:3add-verts!
    #:3build-kdtree
    #:3center!
    #:3edge-length
    #:3export-verts-grp
    #:3get-all-verts
    #:3get-grp-verts
    #:3get-vert
    #:3get-verts
    #:3grp-transform!
    #:3import-verts-grp
    #:3itr-edge-verts
    #:3itr-edge-verts*
    #:3ledge-length
    #:3lsplit-edge!
    #:3lsplit-edge?
    #:3make-vert-getter
    #:3move-vert!
    #:3move-vert?
    #:3prune-edges-by-len!
    #:3relative-neighborhood!
    #:3split-edge!
    #:3split-edge?
    #:3transform!
    #:3vadd-edge?
    #:3verts-in-rad
    #:add-edge!
    #:add-edge?
    #:add-edges!
    #:add-grp!
    #:add-path!
    #:add-path-ind!
    #:add-vert!
    #:add-vert?
    #:add-verts!
    #:append-edge?
    #:build-zonemap
    #:center!
    #:del-edge!
    #:del-edge?
    #:edge-exists
    #:edge-length
    #:export-verts-grp
    #:get-all-grps
    #:get-all-verts
    #:get-continous-paths
    #:get-cycle-basis
    #:get-edges
    #:get-grp
    #:get-grp-num-verts
    #:get-grp-props
    #:get-grp-verts
    #:get-incident-edges
    #:get-incident-rotated-vert
    #:get-incident-verts
    #:get-min-spanning-tree
    #:get-num-edges
    #:get-num-grps
    #:get-num-verts
    #:get-planar-cycles
    #:get-spanning-tree
    #:get-vert
    #:get-vert-by-name
    #:get-vert-ind-by-name
    #:get-vert-inds
    #:get-vert-inds-by-name
    #:get-verts
    #:get-verts-by-name
    #:get-west-most-vert
    #:grp-transform!
    #:import-verts-grp
    #:intersect-all!
    #:is-vert-in-grp
    #:itr-edge-verts
    #:itr-edge-verts*
    #:itr-edges
    #:itr-grp-verts
    #:itr-grps
    #:itr-verts
    #:ladd-edge!
    #:ladd-edge?
    #:ldel-edge!
    #:ldel-edge?
    #:ledge-length
    #:lsplit-edge!
    #:lsplit-edge-ind!
    #:lsplit-edge-ind?
    #:lsplit-edge?
    #:make
    #:make-vert-getter
    #:move-vert!
    #:move-vert?
    #:prune-edges-by-len!
    #:relative-neighborhood!
    #:set-grp-props!
    #:split-edge!
    #:split-edge-ind!
    #:split-edge-ind?
    #:split-edge?
    #:transform!
    #:vadd-edge?
    #:verts-in-rad
    #:with
    #:with-rnd-edge
    #:with-rnd-vert
    #:with-verts-in-rad)
  (:import-from #:weir-utils
    #:*opt-settings*
    #:make-adjustable-vector
    #:to-list
    #:to-vector
    #:vextend
    #:with-struct))
