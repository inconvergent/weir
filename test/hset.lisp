#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(load "../utils/test")

(defun test-hset ()

  (let ((hs (hset:make)))

    (do-test (hset:add hs 1) t)

    (do-test (hset:add hs 1) nil)

    (do-test (hset:add hs 20) t)

    (do-test (hset:add hs 40) t)

    (do-test (hset:add hs 73) t)

    (do-test (hset:num hs) 4)

    (do-test (hset:del hs 1) t)

    (do-test (hset:del hs 1) nil)

    (do-test (hset:mem hs 40) t)

    (do-test (hset:mem* hs (list 40 88)) (list t nil))

    (do-test (sort (hset:to-list hs) #'<) (list 20 40 73)))

  (let ((hs (hset:make :init (list 1 2 3))))
    (do-test (hset:to-list hs) (list 1 2 3))))



(defun main ()
  (test-title (test-hset))
  (test-title (test-summary)))

(main)

