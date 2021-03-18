(in-package #:weir-tests)

(defun test-weir3 (wer)

  (do-test (weir:3add-vert! wer (vec:3vec 0d0 0d0 3d0)) 0)

  (do-test (weir:3add-vert! wer (vec:3vec 10d0 0d0 4d0)) 1)

  (do-test (weir:3add-vert! wer (vec:3vec 3d0 3d0 1d0)) 2)

  (do-test (weir:3add-vert! wer (vec:3vec 4d0 3d0 4d0)) 3)

  (do-test (weir:3add-vert! wer (vec:3vec 7d0 200d0 1d0)) 4)

  (do-test (weir:3add-vert! wer (vec:3vec 2d0 10d0 4d0)) 5)

  (do-test (weir:3get-vert wer 2) (vec:3vec 3d0 3d0 1d0))

  (do-test (weir:ladd-edge! wer '(0 1)) (list 0 1))

  (do-test (weir:3ledge-length wer '(0 1)) 10.04987562112089d0)

  (do-test (weir:3move-vert! wer 3 (vec:3vec 1d0 3d0 3d0) :ret t) (vec:3vec 5d0 6d0 7d0))

  (do-test (weir:3move-vert! wer 4 (vec:3vec 0.5d0 0.6d0 1d0) :rel t :ret t)
           (vec:3vec 7.5d0 200.6d0 2d0)))


(defun test-weir3-with ()
  (let ((wer (weir:make :dim 3)))

    (weir:with (wer %)
      (% (weir:3add-vert? (vec:3vec 11d0 3d0 9d0)))
      (list 4.5
            (% (weir:3move-vert? 0 (vec:3vec 1d0 0d0 9d0)))
            nil
            t
            (list 5 (% (weir:3add-vert? (vec:3vec 12d0 3d0 3d0)))
                    (% (weir:3add-vert? (vec:3vec 13d0 3d0 2d0))))
            (list nil)
            (list (list))))

    (do-test (weir:get-num-verts wer) 3))

    (let ((wer (weir:make :dim 3)))

      (weir:with (wer %)
        (list)
        1 nil
        (% (weir:3add-vert? (vec:3vec 12d0 3d0 2d0)))
        (% (weir:3add-vert? (vec:3vec 13d0 6d0 3d0)))
        (% (weir:3add-vert? (vec:3vec 13d0 3d0 3d0))))

      (weir:with (wer %)
        (% (weir:add-edge? 1 2))
        (% (weir:add-edge? 0 1)))

      (do-test (weir:edge-exists wer '(0 1)) t)

      (do-test (weir:3get-vert wer 2) (vec:3vec 12d0 3d0 2d0))
      (do-test (weir:3get-vert wer 0) (vec:3vec 13d0 3d0 3d0))

      (do-test (weir:edge-exists wer '(1 2)) t)
      (do-test (weir:edge-exists wer '(7 2)) nil)))


(defun test-weir3-split ()
  (let ((wer (weir:make :dim 3)))

    (weir:3add-vert! wer (vec:3vec 0d0 3d0 6d0))
    (weir:3add-vert! wer (vec:3vec 1d0 4d0 7d0))
    (weir:3add-vert! wer (vec:3vec 2d0 5d0 8d0))
    (weir:add-edge! wer 0 1)
    (weir:add-edge! wer 1 2)
    (weir:add-edge! wer 2 0)

    (weir:with (wer %)
      (% (weir:3split-edge? 0 1 :xy (vec:3vec 30d0 20d0 3d0)) :res :a)
      (% (weir:3lsplit-edge? '(1 2) :xy (vec:3vec 31d0 23d0 4d0)) :res :b)
      (% (weir:3lsplit-edge? '(2 1) :xy (vec:3vec 32d0 24d0 5d0)) :res :c))

    (do-test (sort-a-list (weir:get-alteration-result-list wer))
             '((:a 4) (:b nil) (:c 3)))

    (do-test (weir:3get-vert wer 3) (vec:3vec 32d0 24d0 5d0))))


(defun test-weir3-kdtree ()
  (rnd:set-rnd-state 2)
  (let ((wer (weir:make :dim 3)))

    (loop repeat 2000 do (weir:3add-vert! wer (rnd:3in-cube 1000d0)))

    (weir:3build-kdtree wer)

    (do-test
      (sort (weir:3verts-in-rad wer (vec:3vec 20d0 200d0 43d0) 100d0) #'<)
      '#(1340 1541))))



(define-file-tests test-weir3-galore ()
  (test-title (test-weir3 (weir:make :dim 3)))
  (test-title (test-weir3-with))
  (test-title (test-weir3-split))
  (test-title (test-weir3-kdtree)))

