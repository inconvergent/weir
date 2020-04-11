#!/usr/bin/sbcl --script

(load "../src/load")
(asdf:load-system "weir")
(load "../utils/test")

(setf *print-pretty* t)
(rnd:set-rnd-state 1)


(defun test-ortho ()
  (let* ((proj (ortho:make :cam (rnd:3on-sphere :rad 1000d0)
                           :look (vec:3rep 0d0)
                           :s 0.3d0
                           :up (rnd:3on-sphere :rad 1d0)
                           :xy (vec:rep 500d0)))
         (sphere (rnd:3in-sphere :rad 1000d0))
         (pts (math:nrep 10 (rnd:3in-sphere :rad 1000d0)))
         (proj2 (ortho:import-data (ortho:export-data proj))))

    (do-test (ortho:project proj sphere)
             #s(vec:vec :x 428.26535543321796d0 :y 413.50713561727366d0))

    (do-test (ortho:project* proj pts)
             '((#s(vec:vec :x 368.74896389212864d0 :y 280.279598957188d0) 1305.1359664407541d0)
               (#s(vec:vec :x 422.7747896862477d0 :y 759.894952721563d0) 774.1574761075917d0)
               (#s(vec:vec :x 340.1637221124419d0 :y 601.1791164318078d0) 504.2327213069508d0)
               (#s(vec:vec :x 584.4022680047473d0 :y 465.60940751781754d0) 605.6901110310558d0)
               (#s(vec:vec :x 473.21193676547676d0 :y 530.7950575570878d0) 854.8853502297287d0)
               (#s(vec:vec :x 551.8525560829912d0 :y 566.5564812868154d0) 1452.1762487835977d0)
               (#s(vec:vec :x 303.8705766210252d0 :y 299.92988593987775d0) 1288.9462886925376d0)
               (#s(vec:vec :x 490.33700083746317d0 :y 311.2203720974637d0) 349.7004840498294d0)
               (#s(vec:vec :x 428.3508272389861d0 :y 319.37297224061194d0) 821.4113295304979d0)
               (#s(vec:vec :x 661.854180254181d0 :y 485.4092153081456d0) 178.85371424194935d0)))

    (do-test (ortho:project* proj pts)
             '((#s(vec:vec :x 368.74896389212864d0 :y 280.279598957188d0) 1305.1359664407541d0)
               (#s(vec:vec :x 422.7747896862477d0 :y 759.894952721563d0) 774.1574761075917d0)
               (#s(vec:vec :x 340.1637221124419d0 :y 601.1791164318078d0) 504.2327213069508d0)
               (#s(vec:vec :x 584.4022680047473d0 :y 465.60940751781754d0) 605.6901110310558d0)
               (#s(vec:vec :x 473.21193676547676d0 :y 530.7950575570878d0) 854.8853502297287d0)
               (#s(vec:vec :x 551.8525560829912d0 :y 566.5564812868154d0) 1452.1762487835977d0)
               (#s(vec:vec :x 303.8705766210252d0 :y 299.92988593987775d0) 1288.9462886925376d0)
               (#s(vec:vec :x 490.33700083746317d0 :y 311.2203720974637d0) 349.7004840498294d0)
               (#s(vec:vec :x 428.3508272389861d0 :y 319.37297224061194d0) 821.4113295304979d0)
               (#s(vec:vec :x 661.854180254181d0 :y 485.4092153081456d0) 178.85371424194935d0)))))


(defun main ()
  (test-title (test-ortho))
  (test-title (test-summary)))

(main)

