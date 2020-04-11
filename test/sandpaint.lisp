#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(load "../utils/test")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun get-sample-pix (sand)
  (let ((vals (sandpaint::sandpaint-vals sand))
        (indfx (sandpaint::sandpaint-indfx sand)))
    (alexandria:flatten
      (loop for i in '(10 45 45 92 23)
            and j in '(39 78 49 92 89)
            collect (list (aref vals (funcall indfx i j 0))
                          (aref vals (funcall indfx i j 1))
                          (aref vals (funcall indfx i j 2))
                          (aref vals (funcall indfx i j 3)))))))


(defun test-sandpaint ()
  (let ((sand (sandpaint:make :size 100
                              :fg (pigment:green)
                              :bg (pigment:black))))
    (loop for p in (rnd:nin-rect 500000 30d0 30d0 :xy (vec:vec 50d0 50d0)) do
          (sandpaint:set-fg sand (pigment:rgb (rnd:rnd) (rnd:rnd)
                                              (rnd:rnd) 0.004d0))
          (sandpaint:pix sand (list p)))

    (loop for p in (rnd:nin-rect 500000 30d0 30d0 :xy (vec:vec 60d0 50d0)) do
          (sandpaint:set-fg sand (pigment:rgb (rnd:rnd) (rnd:rnd)
                                              (rnd:rnd) 0.004d0))
          (sandpaint:pix sand (list p)))

    (loop for p in (rnd:nin-rect 500000 30d0 30d0 :xy (vec:vec 50d0 60d0)) do
          (sandpaint:set-fg sand (pigment:rgb (rnd:rnd) (rnd:rnd)
                                              (rnd:rnd) 0.004d0))
          (sandpaint:pix sand (list p)))

    (do-test
      (get-sample-pix sand)
      '(0.0d0 0.0d0 0.0d0 1.0d0 0.39317154020166095d0 0.4106421499762462d0
        0.3773218130882492d0 1.0d0 0.3975273152203998d0 0.4253778247192912d0
        0.42339799985131144d0 1.0d0 0.0d0 0.0d0 0.0d0 1.0d0
        0.22647985791951575d0 0.2203285988306688d0 0.22192693772741898d0
        1.0d0))

    (sandpaint:save sand "data/sandpaint-rnd"))

  (let ((sand (sandpaint:make :size 100
                              :fg (pigment:black)
                              :bg (pigment:transparent))))

    (sandpaint:set-fg sand (pigment:red 0.1d0))
    (sandpaint:pix sand (rnd:nin-rect 30000 40d0 50d0 :xy (vec:vec 50d0 50d0)))

    (sandpaint:set-fg sand (pigment:green 0.1d0))
    (sandpaint:pix sand (rnd:nin-rect 30000 50d0 20d0 :xy (vec:vec 50d0 50d0)))

    (sandpaint:set-fg sand (pigment:blue 0.1d0))
    (sandpaint:pix sand (rnd:nin-rect 30000 20d0 50d0 :xy (vec:vec 50d0 50d0)))

    (do-test
      (get-sample-pix sand)
      '(0.11991051555039006d0 0.6513215599000001d0 0.0d0 0.7712320754503901d0
        0.18152935690535107d0 0.0d0 0.6125795110000001d0 0.7941088679053511d0
        0.14013854335308507d0 0.24952897545039007d0 0.5217031000000001d0
        0.911370618803475d0 0.0d0 0.0d0 0.0d0 0.0d0 0.271d0 0.0d0 0.0d0
        0.271d0))

    (sandpaint:save sand "data/sandpaint-8")
    (sandpaint:save sand "data/sandpaint-16" :bits 16))

  (let ((sand (sandpaint:make :size 300
                              :fg (pigment:red 0.1d0)
                              :bg (pigment:white))))

    (sandpaint:pix sand (rnd:nin-circ 100000 50d0 :xy (vec:vec 0d0 150d0)))
    (sandpaint:pix sand (rnd:nin-circ 100000 50d0 :xy (vec:vec 150d0 150d0)))
    (sandpaint:pix sand (rnd:nin-circ 100000 50d0 :xy (vec:vec 300d0 150d0)))

    (sandpaint:save sand "data/sandpaint-circ")))


(defun main ()
  (test-title (test-sandpaint))
  (test-title (test-summary)))

(main)

