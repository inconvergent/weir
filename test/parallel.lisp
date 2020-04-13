(in-package #:weir-tests)

(defun test-par ()
  (parallel:init)
  (parallel:info)
  (parallel:end))


(define-file-tests test-parallel ()
  (test-title (test-par)))
