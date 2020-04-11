#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(load "../utils/test")

(defun test-par ()

  (parallel:init)
  (parallel:info)
  (parallel:end)

  )


(defun main ()
  (test-title (test-par))
  (test-title (test-summary)))

(main)

