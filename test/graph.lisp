(in-package #:weir-tests)

(defun %test-graph ()

  (let ((grph (graph:make)))

    (do-test (graph:add grph 1 1) t)

    (do-test (graph:add grph 1 2) t)

    (do-test (graph:add grph 1 2) nil)

    (do-test (graph:add grph 2 1) nil)

    (do-test (graph:get-num-edges grph) 4)

    (do-test (graph:get-edges grph) '((1 2) (1 1)))

    (do-test (graph:add grph 20 5) t)

    (do-test (graph:get-edges grph) '((5 20) (1 2) (1 1)))

    (do-test (graph:del grph 1 2) t)

    (do-test (graph:del grph 1 2) nil)

    (do-test (graph:get-edges grph) '((5 20) (1 1)))

    (do-test (graph:get-num-edges grph) 4)

    (do-test (graph:mem grph 1 4) nil)

    (do-test (graph:mem grph 1 1) t)

    (do-test (sort (graph:get-verts grph) #'<) '(1 5 20))

    (do-test (graph:del grph 1 1) t)

    (do-test (graph:get-edges grph) '((5 20)))

    (do-test (sort (graph:get-verts grph) #'<) '(5 20))

    (do-test (graph:del grph 5 20) t)

    (do-test (sort (graph:get-verts grph) #'<) nil))

  (let ((grph (graph:make)))
    ; ensure that mutating one graph does not effect the other
    (graph:add grph 2 1)
    (graph:add grph 3 2)
    (graph:add grph 4 1)

    (let ((new-grph (graph:copy grph)))
      (graph:del new-grph 1 4)

      (do-test (length (graph:get-edges grph)) 3)

      (do-test (length (graph:get-edges new-grph)) 2)))

  (let ((grph (graph:make)))
    (graph:add grph 0 1)
    (graph:add grph 3 2)
    (graph:add grph 1 3)
    (graph:add grph 0 3)
    (graph:add grph 1 4)
    (graph:add grph 4 5)
    (graph:add grph 5 6)

    (do-test (length (graph:get-edges (graph:del-simple-filaments grph))) 3)))


(define-file-tests test-graph ()
  (test-title (%test-graph)))
