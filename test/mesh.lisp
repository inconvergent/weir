(in-package #:weir-tests)

(defun get-mdst (ll)
  (loop with res = 1000d0
        for i from 0
        for a in ll
        do (loop for j from 0
                 for b in ll
                 do (let ((d (vec:3dst a b)))
                      (when (and (not (= i j)) (< d res))
                            (setf res d))))
        finally (return res)))

(defun make-polyhedra (msh &key (rad 400d0) (xy vec:*3zero*))
  (destructuring-bind (a b c d)
    (mesh:add-verts! msh
      (loop with mdst = -1d0
            with res = nil
            until (> mdst (* rad 1.5d0))
            do (setf res (math:nrep 4 (rnd:3on-sphere :rad rad :xy xy))
                     mdst (get-mdst res))
            finally (return res)))
    (mesh:add-polygon! msh (list a b c))
    (mesh:add-polygon! msh (list a b d))
    (mesh:add-polygon! msh (list a c d))
    (mesh:add-polygon! msh (list d b c)))
  msh)


(defun %test-mesh ()

  (let ((msh (mesh:make)))

    (do-test (mesh:add-vert! msh (vec:3vec 0d0 0.5d0 0.1d0)) 0)
    (do-test (mesh:add-vert! msh (vec:3vec 1d0 1.3d0 1.1d0)) 1)

    (do-test (mesh:add-verts! msh (list (vec:3vec 1d0 2.3d0 2d0)
                                        (vec:3vec 1d0 3.1d0 3d0)))
             '(2 3))

    (do-test (mesh:add-vert! msh (vec:3vec 3d0 3d0 9d0)) 4)

    (do-test (mesh:get-vert msh 0) (vec:3vec 0d0 0.5d0 0.1d0))

    (do-test (mesh:add-polygon! msh '(0 2 1)) '(0 2 1))

    (do-test (mesh:add-polygon! msh '(2 3 1)) '(1 2 3))

    (do-test (mesh:add-polygon! msh '(2 1 3)) '(1 2 3))

    (do-test (mesh:get-all-polygons msh)
             '((0 2 1) (1 2 3)))

    (do-test (mesh:add-polygon! msh '(3 2 4)) '(2 4 3))

    (do-test (mesh:get-num-polygons msh) 3)
    (do-test (mesh:get-num-edges msh) 7)
    (do-test (mesh:get-num-verts msh) 5)

    (do-test (mesh:edge-length msh '(3 2)) 1.28062484748657d0)

    (do-test (mesh:normal msh '(1 2 3)) (vec:3vec 1d0 0d0 0d0))

    ; TODO: below tests are not actually stable, probably
    (do-test (mesh:get-all-edges msh :extra t)
             '(((0 2) ((0 2 1)))
               ((1 2) ((1 2 3) (0 2 1)))
               ((0 1) ((0 2 1)))
               ((2 3) ((2 4 3) (1 2 3)))
               ((1 3) ((1 2 3)))
               ((2 4) ((2 4 3)))
               ((3 4) ((2 4 3)))))
    (do-test (mesh:get-polygon-edges msh '(1 2 3))
             '((1 2) (2 3) (1 3)))

    (do-test (mesh:get-polygon-edges msh '(1 2 9) :err nil)
             '())

    (do-test (mesh:get-edge-polygons msh '(1 2))
             '((1 2 3) (0 2 1)))))

(defun test-mesh-2 ()
  (let ((msh (mesh:make)))

    (mesh:add-rect! msh :sx 10d0 :sy 20d0)

    (do-test (mesh:get-all-polygons msh) '((1 3 2) (0 3 1)))

    (do-test (mesh:del-polygon! msh '(1 2 3)) t)

    (do-test (mesh:del-polygon! msh '(1 2 3)) nil)
    (do-test (mesh:del-polygon! msh '(1 3 2)) nil)
    (do-test (mesh:get-all-edges msh :extra t)
             '(((1 3) ((0 3 1)))
               ((0 3) ((0 3 1)))
               ((0 1) ((0 3 1)))))

    (do-test (mesh:del-polygon! msh '(0 3 1)) t)
    (do-test (mesh:get-all-edges msh :extra t)
             nil)))

(defun test-rt ()
  (let ((msh (make-polyhedra (mesh:make)))
        (lines (loop repeat 5
                     collect
                     (list (rnd:3in-sphere :rad 400d0 :xy (vec:3rep 1000d0))
                           (rnd:3in-sphere :rad 400d0 :xy (vec:3rep -1000d0))))))

    (do-test
      (loop with vertfx = (mesh:make-vert-getter msh)
            with bvh = (mesh:make-bvh msh :vertfx vertfx)
            with raycastfx = (mesh:make-raycaster bvh)
            for line in lines
            collect (funcall raycastfx line))
      '(#s(mesh:bvhres
        :i (0 1 3)
        :s 0.45577317191322947d0
        :pt #s(vec:3vec :x 68.35576641396074d0 :y 129.72873881509554d0 :z 162.99471151764772d0)
        :n #s(vec:3vec :x 0.7199099028745667d0 :y 0.6742471705021035d0 :z -0.1646829827670099d0))
      #s(mesh:bvhres
         :i (0 1 3)
         :s 0.49454805601282065d0
         :pt #s(vec:3vec :x -8.030407004566769d0 :y 187.49124501419692d0 :z 65.56535936935938d0)
         :n #s(vec:3vec :x 0.7199099028745667d0 :y 0.6742471705021035d0 :z -0.1646829827670099d0))
      #s(mesh:bvhres
         :i (0 1 3)
         :s 0.4094759469790495d0
         :pt #s(vec:3vec :x 39.124693445667276d0 :y 158.84988062255388d0 :z 154.4395383456415d0)
         :n #s(vec:3vec :x 0.7199099028745667d0 :y 0.6742471705021035d0 :z -0.1646829827670099d0))
      #s(mesh:bvhres
         :i (-1 -1 -1)
         :s 900000.0d0
         :pt #s(vec:3vec :x 0.0d0 :y 0.0d0 :z 0.0d0)
         :n #s(vec:3vec :x 0.0d0 :y 0.0d0 :z 0.0d0))
      #s(mesh:bvhres
         :i (0 2 3)
         :s 0.47020698603563754d0
         :pt #s(vec:3vec :x 131.25511031357405d0 :y -100.54199887894856d0 :z 27.408671919132985d0)
         :n #s(vec:3vec :x -0.36640614639471814d0 :y 0.40958501157582455d0 :z -0.8354559558568028d0))))))


(define-file-tests test-mesh ()
  (test-title (%test-mesh))
  (test-title (test-mesh-2))
  (test-title (test-rt)))
