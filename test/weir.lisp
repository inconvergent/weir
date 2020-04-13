
(in-package #:weir-tests)

(defun test-weir (wer)

  (do-test (weir:add-vert! wer (vec:vec 0d0 0d0)) 0)

  (do-test (weir:add-vert! wer (vec:vec 10d0 0d0)) 1)

  (do-test (weir:add-vert! wer (vec:vec 3d0 3d0)) 2)

  (do-test (weir:add-vert! wer (vec:vec 4d0 3d0)) 3)

  (do-test (weir:add-vert! wer (vec:vec 7d0 200d0)) 4)

  (do-test (weir:add-vert! wer (vec:vec 2d0 10d0)) 5)

  (do-test (weir:add-vert! wer (vec:vec 4d0 11d0)) 6)

  (do-test (weir:add-vert! wer (vec:vec 3d0 10d0)) 7)

  (do-test (weir:add-vert! wer (vec:vec 0d0 0.5d0)) 8)

  (do-test (weir:add-vert! wer (vec:vec 2d0 1.0d0)) 9)

  (do-test (weir:add-vert! wer (vec:vec 3.0d0 10d0)) 10)

  (do-test (weir:ladd-edge! wer '(0 0)) nil)

  (do-test (weir:ladd-edge! wer '(0 2)) '(0 2))

  (do-test (weir:ladd-edge! wer '(0 1)) '(0 1))

  (do-test (weir:ladd-edge! wer '(5 0)) '(0 5))

  (do-test (weir:ladd-edge! wer '(1 0)) nil)

  (do-test (weir:ladd-edge! wer '(5 0)) nil)

  (do-test (weir:ladd-edge! wer '(0 2)) nil)

  (do-test (weir:add-edge! wer 5 2) '(2 5))

  (do-test (weir:add-edge! wer 4 1) '(1 4))

  (do-test (weir:ladd-edge! wer '(4 0)) '(0 4))

  (do-test (weir:ladd-edge! wer '(5 1)) '(1 5))

  (do-test (weir:ladd-edge! wer '(9 9)) nil)

  (do-test (weir:ladd-edge! wer '(3 9)) '(3 9))

  (do-test (weir:ladd-edge! wer '(0 1)) nil)

  (do-test (weir:ladd-edge! wer '(0 4)) nil)

  (do-test (weir:ladd-edge! wer '(10 9)) '(9 10))

  (do-test (weir:edge-exists wer '(0 2)) t)

  (do-test (weir:edge-exists wer '(5 0)) t)

  (do-test (weir:edge-exists wer '(9 2)) nil)

  (do-test (weir:edge-exists wer '(2 2)) nil)

  (do-test (weir:get-vert wer 2) (vec:rep 3.0d0))

  (do-test (weir:add-vert! wer (vec:vec 0d0 1d0)) 11)

  (do-test (weir:ladd-edge! wer '(0 1)) nil)

  (do-test (weir:add-vert! wer (vec:vec 0d0 7d0)) 12)

  (do-test (weir:ledge-length wer '(0 4)) 200.12246250733574d0)

  (do-test (weir:edge-length wer 2 5) 7.0710678118654755d0)

  (do-test (weir:ledge-length wer '(1 2)) 7.615773105863909d0)

  (do-test (weir:move-vert! wer 3 (vec:vec 1d0 3d0) :ret t) (vec:vec 5d0 6d0))

  (do-test (weir:move-vert! wer 3 (vec:vec 0.5d0 0.6d0) :rel t :ret t)
           (vec:vec 5.5d0 6.6d0))

  (do-test (weir:get-vert wer 3) (vec:vec 5.5d0 6.6d0)))

(defun test-weir-2 (wer)

  (do-test (weir:add-vert! wer (vec:rep 0d0)) 0)

  (do-test (weir:add-vert! wer (vec:rep 20d0)) 1)

  (do-test (weir:add-vert! wer (vec:vec 30d0 30d0)) 2)

  (do-test (weir:add-vert! wer (vec:vec 40d0 40d0)) 3)

  (do-test (weir:ladd-edge! wer '(0 1)) '(0 1))

  (do-test (weir:ladd-edge! wer '(1 2)) '(1 2))

  (do-test (weir:ladd-edge! wer '(2 3)) '(2 3))

  (do-test (weir:ladd-edge! wer '(3 1)) '(1 3))

  (do-test (weir:get-edges wer) '((2 3) (1 3) (1 2) (0 1)))

  (do-test (weir:del-edge! wer 0 1) t)

  (do-test (weir:ldel-edge! wer '(0 1)) nil)

  (do-test (weir:ldel-edge! wer '(3 2)) t)

  (do-test (weir:ldel-edge! wer '(1 2)) t)

  (do-test (weir:lsplit-edge! wer '(1 2) :xy (vec:vec 1d0 2d0)) nil)

  (do-test (weir:lsplit-edge! wer '(3 1) :xy (vec:vec 1d0 2d0)) 4)

  (do-test (weir:get-num-edges wer) 4)

  (do-test (weir:get-num-verts wer) 5))


(defun test-weir-3 (wer)

  (do-test (weir:add-vert! wer (vec:vec 10d0 10d0)) 0)

  (do-test (weir:add-vert! wer (vec:vec 20d0 10d0)) 1)

  (do-test (weir:add-vert! wer (vec:vec 30d0 10d0)) 2)

  (do-test (weir:add-vert! wer (vec:vec 40d0 10d0)) 3)

  (do-test (weir:ladd-edge! wer '(0 1)) '(0 1))

  (do-test (weir:ladd-edge! wer '(1 2)) '(1 2))

  (do-test (weir:ladd-edge! wer '(2 3)) '(2 3))

  (do-test (weir:ladd-edge! wer '(2 3)) nil))


(defun init-weir ()
  (let ((wer (weir:make :max-verts 16)))
    (weir:add-vert! wer (vec:vec 0d0 2d0)) ;0
    (weir:add-vert! wer (vec:vec 2d0 3d0)) ;1
    (weir:add-vert! wer (vec:vec 3d0 4d0)) ;2
    (weir:add-vert! wer (vec:vec 4d0 7d0)) ;3
    (weir:add-vert! wer (vec:vec 5d0 4d0)) ;4
    (weir:add-vert! wer (vec:vec 0d0 6d0)) ;5
    (weir:add-vert! wer (vec:vec -1d0 7d0)) ;6
    (weir:add-vert! wer (vec:vec 0d0 8d0)) ;7
    (weir:add-vert! wer (vec:vec 0d0 9d0)) ;8
    (weir:add-vert! wer (vec:vec 10d0 1d0)) ;9
    (weir:add-vert! wer (vec:vec 3d0 1d0)) ;10

    (weir:ladd-edge! wer '(1 2))
    (weir:ladd-edge! wer '(0 1))
    (weir:ladd-edge! wer '(3 1))
    (weir:ladd-edge! wer '(5 6))
    (weir:ladd-edge! wer '(7 3))
    wer))


(defun test-weir-incident ()
  (let ((wer (init-weir)))
    (do-test
      (weir:get-incident-edges wer 1)
      '((1 2) (0 1) (1 3)))

    (do-test (weir:get-incident-edges wer 100) nil)))


(defun test-weir-with ()
  (let ((wer (init-weir)))
    (weir:with (wer %)

      (% (weir:add-vert? (vec:vec 11d0 3d0)))
      (list 4.5
            (% (weir:move-vert? 0 (vec:vec 1d0 0d0)))
            nil
            t
            (list 5 (% (weir:add-vert? (vec:vec 12d0 3d0)))
                    (% (weir:add-vert? (vec:vec 13d0 3d0))))
            (list nil)
            (list (list))))

    (do-test (sort (weir:get-vert-inds wer) #'<)
             (list 0 1 2 3 5 6 7)))

    (let ((wer (init-weir)))

      (do-test (weir:edge-exists wer '(7 2)) nil)

      (weir:with (wer %)
        (list)
        1 nil
        (% (weir:add-vert? (vec:vec 12d0 3d0))) ; 12
        (% (weir:add-vert? (vec:vec 13d0 6d0))) ; 11
        (% (weir:add-edge? 1 2))
        (% (weir:add-edge? 2 7))
        (% nil))

      (do-test (weir:get-vert wer 12) (vec:vec 12d0 3d0))
      (do-test (weir:get-vert wer 11) (vec:vec 13d0 6d0))

      (do-test (weir:edge-exists wer '(1 2)) t)
      (do-test (weir:edge-exists wer '(2 7)) t)
      (do-test (weir:edge-exists wer '(7 2)) t)))

(defun test-weir-with-sfx ()
  (let ((wer (weir:make)))
      (weir:add-vert! wer (vec:vec 1d0 1d0))
      (weir:add-vert! wer (vec:vec 2d0 2d0))
      (weir:add-vert! wer (vec:vec 3d0 3d0))

      (weir:with (wer %)
        ; alterations avoid side-effects
        (% (weir:move-vert? 1 (apply #'vec:isub (weir:get-verts wer (list 2 0)))))
        (% (weir:move-vert? 0 (apply #'vec:isub (weir:get-verts wer (list 1 0))))))

      (do-test (weir:get-all-verts wer)
               (list (vec:vec 0.0d0 0.0d0) (vec:vec 0.0d0 0.0d0)
                     (vec:vec 3.0d0 3.0d0))))

    (let ((wer (weir:make)))
      (weir:add-vert! wer (vec:vec 1d0 1d0))
      (weir:add-vert! wer (vec:vec 2d0 2d0))
      (weir:add-vert! wer (vec:vec 3d0 3d0))
      ; this exhibits side-effects
      (weir:move-vert! wer 0 (apply #'vec:isub (weir:get-verts wer (list 1 0))))
      (weir:move-vert! wer 1 (apply #'vec:isub (weir:get-verts wer (list 2 0))))
      (do-test (weir:get-all-verts wer)
               (list (vec:vec 0.0d0 0.0d0) (vec:vec -1.0d0 -1.0d0)
                      (vec:vec 3.0d0 3.0d0))))

    (let ((wer (weir:make)))
      (weir:add-vert! wer (vec:vec 1d0 1d0))
      (weir:add-vert! wer (vec:vec 2d0 2d0))
      (weir:add-vert! wer (vec:vec 3d0 3d0))

      (weir:with (wer %)
        ; calculat a/b outside lambdas to avoid unfortunate side-effects.
        ; this is less elegant than alterations, but quite flexible.
        ; particularly if you design second-order functions
        (let ((a (apply #'vec:isub (weir:get-verts wer (list 2 1))))
              (b (apply #'vec:isub (weir:get-verts wer (list 1 0)))))
          (% (lambda (w) (weir:move-vert! w 1 b)))
          (% (lambda (w) (weir:move-vert! w 0 a)))))

      (do-test (weir:get-all-verts wer)
                (list (vec:vec 0.0d0 0.0d0) (vec:vec 1.0d0 1.0d0)
                      (vec:vec 3.0d0 3.0d0)))))


(defun test-weir-add ()
  (let ((wer (init-weir)))
    (weir:with (wer %)
      (% (weir:add-vert? (vec:vec 10d0 3d0))))

    (do-test (weir:get-vert wer 11) (vec:vec 10d0 3d0))

    (do-test (weir:get-num-verts wer) 12)

    (do-test
      (weir:with (wer % :collect t)
        (% (weir:add-vert? (vec:vec 80d0 3d0)))
        (% (weir:add-vert? (vec:vec 70d0 3d0))))
      '(12 13))

    (do-test (weir:get-num-verts wer) 14)

    (weir:with (wer %)
      (% (weir:vadd-edge? (vec:vec 7d0 3d0) (vec:vec 100d0 0.99d0))))

    (do-test (weir:get-edges wer)
             '((14 15) (5 6) (3 7) (0 1) (1 3) (1 2)))))

(defun test-weir-move ()
  (let ((wer (init-weir)))
    (do-test
      (weir:with (wer % :collect t)
        (% (weir:move-vert? 0 (vec:vec 3d0 3d0)))
        (% (weir:move-vert? 1 (vec:vec 1d0 3d0)))
        (% (weir:move-vert? 3 (vec:vec 2d0 3d0) :rel nil))
        (% (weir:move-vert? 2 (vec:vec 3d0 4d0))))
      (list (vec:vec 6.0d0 8.0d0)
            (vec:vec 2.0d0 3.0d0)
            (vec:vec 3.0d0 6.0d0)
            (vec:vec 3.0d0 5.0d0)))

    (do-test (weir:get-vert wer 0) (vec:vec 3d0 5d0))

    (do-test (weir:get-vert wer 1) (vec:vec 3d0 6d0))

    (do-test (weir:get-vert wer 3) (vec:vec 2d0 3d0))

    (do-test (weir:get-vert wer 2) (vec:vec 6d0 8d0))))

(defun test-weir-join ()
  (let ((wer (init-weir)))
    (weir:with (wer %)
      (% (weir:add-edge? 3 3))
      (% (weir:add-edge? 3 3))
      (% (weir:add-edge? 3 6))
      (% (weir:add-edge? 7 1)))

  (do-test (weir:get-num-edges wer) 14)

  (do-test
    (weir:with (wer % :collect t)
      (% (weir:add-edge? 3 3))
      (% (weir:add-edge? 1 6))
      (% (weir:add-edge? 1 100)))
    '(nil (1 6) nil))))


(defun test-weir-append ()
  (let ((wer (init-weir)))

    (do-test (weir:get-num-verts wer) 11)

    (do-test
      (weir:with (wer % :collect t)
        (% (weir:append-edge? 3 (vec:vec 3d0 4d0)))
        (% (weir:append-edge? 3 (vec:vec 8d0 5d0) :rel nil))
        (% (weir:append-edge? 7 (vec:vec 1d0 2d0))))
      '(11 12 13))

    (do-test (weir:get-num-edges wer) 16)

    (do-test (weir:get-num-verts wer) 14)

    (do-test
      (weir-utils:to-list (weir::weir-verts wer))
      (list 0.0d0 2.0d0 2.0d0 3.0d0 3.0d0 4.0d0 4.0d0 7.0d0 5.0d0 4.0d0
            0.0d0 6.0d0 -1.0d0 7.0d0 0.0d0 8.0d0 0.0d0 9.0d0 10.0d0 1.0d0
            3.0d0 1.0d0 1.0d0 10.0d0 8.0d0 5.0d0 7.0d0 11.0d0 0.0d0 0.0d0
            0.0d0 0.0d0))))


(defun test-weir-split ()
  (let ((wer (init-weir)))
    (do-test
      (weir:with (wer % :collect t)
        (% (weir:split-edge? 1 2 :xy (vec:vec 30d0 20d0)))
        (% (weir:lsplit-edge? '(1 2) :xy (vec:vec 31d0 23d0)))
        (% (weir:lsplit-edge? '(5 6) :xy (vec:vec 32d0 24d0))))
      '(11 12 nil))

  (do-test (weir:get-num-edges wer) 14)

  (do-test (weir:get-num-verts wer) 13)

  (do-test
    (weir-utils:to-list (weir::weir-verts wer))

    (list 0.0d0 2.0d0 2.0d0 3.0d0 3.0d0 4.0d0 4.0d0 7.0d0 5.0d0 4.0d0 0.0d0
          6.0d0 -1.0d0 7.0d0 0.0d0 8.0d0 0.0d0 9.0d0 10.0d0 1.0d0 3.0d0 1.0d0
          32.0d0 24.0d0 31.0d0 23.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0 0.0d0))))


(defun test-weir-itrs ()
  (let ((wer (init-weir)))
    (weir:with (wer %)
      (weir:with-rnd-vert (wer v)
        (% (weir:move-vert? v (vec:vec 2d0 2d0)))
        (% (weir:append-edge? v (vec:vec 3d0 2d0)))))

    (do-test (weir:get-num-edges wer) 12)

    (do-test (weir:get-num-verts wer) 12)

    (do-test (weir::weir-wc wer) 1)

    (weir:with (wer %)
      (weir:itr-verts (wer v)
        (% (weir:move-vert? v (vec:vec 2d0 2d0)))))

    (do-test
      (sort (alexandria:flatten (weir:itr-verts (wer i :collect t) i)) #'<)
      '(0 1 2 3 4 5 6 7 8 9 10 11))

    (do-test
      (alexandria:flatten (weir:itr-verts (wer i) i))
      nil)

    (do-test
      (sort (alexandria:flatten (weir:itr-grp-verts (wer i :collect t) i)) #'<)
      '(0 1 2 3 5 6 7 11))

    (do-test
      (weir:itr-edges (wer e :collect t) e)
      '(((5 11)) ((5 6)) ((3 7)) ((0 1)) ((1 3)) ((1 2))))

    (do-test
      (sort (alexandria:flatten
              (weir:itr-edges (wer e :collect t) (weir:ledge-length wer e))) #'<)
      '(1.0d0 1.4142135623730951d0 2.23606797749979d0 3.1622776601683795d0
        4.123105625617661d0 4.47213595499958d0))

    (do-test (weir::weir-wc wer) 2)

    (weir:with (wer %)
      (weir:with-rnd-edge (wer e)
        (% (weir:lsplit-edge? e :xy (vec:vec 31d0 23d0)))))

    (do-test (weir:get-num-edges wer) 14)

    (do-test (weir:get-num-verts wer) 13)))

(defun test-weir-zonemap ()
  (let ((wer (weir:make)))

    (weir:add-vert! wer (vec:vec 100d0 200d0))
    (weir:add-vert! wer (vec:vec 200d0 300d0))
    (weir:add-vert! wer (vec:vec 300d0 400d0))
    (weir:add-vert! wer (vec:vec 400d0 500d0))
    (weir:add-vert! wer (vec:vec 500d0 600d0))
    (weir:add-vert! wer (vec:vec 600d0 700d0))
    (weir:add-vert! wer (vec:vec 700d0 800d0))
    (weir:add-vert! wer (vec:vec 800d0 900d0))

    (zonemap:make (weir::weir-verts wer)
               (weir:get-num-verts wer)
               100.0d0)

    (weir:with (wer % :zwidth 50.0d0)
      (do-test
        (sort (weir:verts-in-rad wer (vec:vec 500d0 500d0) 50.0d0) #'<)
        #())

      (do-test
        (sort (weir:verts-in-rad wer (vec:vec -500d0 500d0) 50.0d0) #'<)
        #()))

    (weir:with (wer % :zwidth 200.0d0)
      (do-test
        (sort (weir:verts-in-rad wer (vec:vec 800d0 800d0) 200.0d0) #'<)
        #(6 7))

      (do-test
        (let ((a (list)))
          (weir:with-verts-in-rad (wer (vec:rep 800d0) 200d0 v)
            (setf a (append a (list v))))
          a)
        (list 6 7))

      (do-test
        (sort (weir:verts-in-rad wer (vec:rep 500d0) 200.0d0) #'<)
        #(3 4))

      (do-test
        (let ((a (list)))
          (weir:with-verts-in-rad (wer (vec:rep 500d0) 200d0 v)
            (setf a (append a (list v))))
          a)
        (list 3 4)))

    (weir:with (wer % :zwidth 1000.0d0)
      (do-test
        (sort (weir:verts-in-rad wer (vec:rep 500d0) 1000.0d0) #'<)
        #(0 1 2 3 4 5 6 7))))

  (let ((wer (weir:make)))

    (weir:add-verts! wer (rnd:nin-circ 330 400d0 :xy (vec:rep 0d0)))

    (weir:with (wer % :zwidth 50d0)
      (do-test
        (sort (weir:verts-in-rad wer (vec:vec 0d0 30d0) 20d0) #'<)
        #(107 220))

      (do-test
        (sort (weir:verts-in-rad wer (vec:rep 0d0) 50d0) #'<)
        #(60 62 107 183 220)))))


(defun test-weir-grp ()
  (let ((wer (weir:make :max-verts 22 :adj-size 30)))

    (let ((g1 (weir:add-grp! wer :type 'path))
          (g2 (weir:add-grp! wer))
          (g3 (weir:add-grp! wer :type 'path)))
      (weir:add-vert! wer (vec:vec 100d0 200d0))
      (weir:add-vert! wer (vec:vec 200d0 300d0))
      (weir:add-vert! wer (vec:vec 300d0 400d0))
      (weir:add-vert! wer (vec:vec 400d0 500d0))
      (weir:add-vert! wer (vec:vec 600d0 700d0))
      (weir:add-vert! wer (vec:vec 700d0 800d0))
      (weir:add-vert! wer (vec:vec 800d0 900d0))
      (weir:add-vert! wer (vec:vec 500d0 600d0))
      (weir:add-vert! wer (vec:vec 900d0 600d0))

      (weir:ladd-edge! wer '(1 2) :g g1)
      (weir:ladd-edge! wer '(1 2))
      (weir:ladd-edge! wer '(1 2) :g g2)
      (weir:ladd-edge! wer '(3 2) :g g2)
      (weir:ladd-edge! wer '(1 5) :g g3)

      (do-test (sort (alexandria:flatten
                       (weir:itr-grp-verts (wer i :g g2 :collect t) i)) #'<)
               '(1 2 3))

      (do-test (sort (alexandria:flatten
                       (weir:itr-grp-verts (wer i :g nil :collect t) i)) #'<)
               '(1 2))

      (do-test (sort (alexandria:flatten
                       (weir:itr-edges (wer e :g g1 :collect t) e)) #'<) '(1 2))

      (do-test (sort (weir:get-vert-inds wer :g g1) #'<) '(1 2))

      (do-test (sort (weir:get-vert-inds wer :g g3) #'<) '(1 5))

      (do-test (length (weir:get-vert-inds wer)) 2)

      (do-test (length (weir:itr-grps (wer g :collect t) g)) 3))))

(defun test-weir-loop ()
  (let ((wer (weir:make)))
    (weir:add-path! wer (list (vec:vec 0d0 0d0) (vec:vec 10d0 0d0)
                              (vec:vec 10d0 10d0) (vec:vec 0d0 10d0))
                    :closed t)

    (do-test (weir:get-west-most-vert wer) 0)

    (weir:add-path! wer (list (vec:vec -10d0 -1d0) (vec:vec 10d0 10d0)))

    (do-test (weir:get-west-most-vert wer) 4))

  (let ((wer (weir:make)))
    (weir:add-verts! wer (list (vec:vec 0d0 0d0) (vec:vec 1d0 0d0)
                               (vec:vec 1d0 -1d0) (vec:vec 1d0 1d0)))

    (weir:add-edge! wer 0 1)
    (weir:add-edge! wer 0 2)
    (weir:add-edge! wer 0 3)

    (do-test (weir:get-incident-rotated-vert wer 0 :dir :cw) 2)
    (do-test (weir:get-incident-rotated-vert wer 0 :dir :ccw) 3))

  (let ((wer (weir:make)))
    (weir:add-verts! wer (bzspl:adaptive-pos
                           (bzspl:make (rnd:nin-circ 10 100d0))
                           :lim 1d0))

    (do-test (weir:get-num-verts wer) 95)

    (do-test (weir:get-num-edges wer) 0)

    (weir:relative-neighborhood! wer 500d0)

    (do-test (weir:get-num-edges wer) 210)

    (weir:add-path! wer (vec:polygon 4 20d0))

    (weir:add-path! wer (vec:polygon 4 20d0) :closed t)

    (do-test (weir:get-planar-cycles wer)
             '((56 43 44 45 46 47 48 49 50 51 52 53 54 55 56) (60 87 86 85 60)
               (18 89 62 61 88 87 60 84 16 83 17 18)
               (11 12 13 14 15 16 84 60 85 59 58 57 42 41 40 39 11)
               (91 66 65 64 90 20 21 91) (80 92 69 68 67 91 21 22 23 24 80)
               (101 102 99 100 101)
               (25 24 23 22 21 20 90 19 89 18 17 83 16 15 14 13 12 11 10 9 8 7 6 5 4 3 25)
               (1 80 24 25 2 1) (0 71 70 92 80 1 79 0)
               (30 29 28 27 26 2 25 3 4 5 6 37 36 35 34 33 32 31 30)
               (75 74 73 72 0 78 77 76 75)))

    (do-test (weir:get-continous-paths wer :cycle-info nil)
             '((0 71) (0 72) (0 78 77 76 75 74 73 72) (0 79 1) (1 2) (1 80)
              (2 25) (2 26 27 28 29 30 31 32 33 34 35 36 37 6) (6 5 4 3 25)
              (6 7 8 9 10) (10 11) (10 38) (11 12 13 14 15 16) (11 39 40 41 42)
              (16 83 17 18 89) (16 84 60) (20 21) (20 82) (20 90) (21 22) (21 91)
              (22 23 24) (22 81) (24 25) (24 80) (42 56) (42 57 58 59 85)
              (56 43 44 45 46 47 48 49 50 51 52 53 54 55 56) (60 85) (60 87)
              (63 89) (71 70 92) (71 93) (72 94) (80 92) (85 86 87) (87 88 61 62 89)
              (89 19 90) (90 64 65 66 91) (91 67 68 69 92)
              (95 96 97 98) (99 102 101 100 99)))))


(define-file-tests test-weir-galore ()
  (test-title (test-weir (weir:make)))
  (test-title (test-weir-2 (weir:make)))
  (test-title (test-weir-3 (weir:make)))
  (test-title (test-weir-incident))
  (test-title (test-weir-with))
  (test-title (test-weir-with-sfx))
  (test-title (test-weir-add))
  (test-title (test-weir-move))
  (test-title (test-weir-join))
  (test-title (test-weir-append))
  (test-title (test-weir-split))
  (test-title (test-weir-itrs))
  (test-title (test-weir-zonemap))
  (test-title (test-weir-grp))
  (test-title (test-weir-loop)))
