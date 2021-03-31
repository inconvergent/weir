(in-package #:weir-tests)


(defun %test-vec ()

  (do-test (vec:norm (vec:vec 3.0d0 0.0d0))
           (vec:vec 1.0d0 0.0d0))

  (do-test (vec:sub (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 3.0d0))
           (vec:vec -1.d0 -1.d0))

  (do-test (vec:add (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 3.0d0))
           (vec:vec 3.0d0 5.0d0))

  (do-test (vec:nsub (vec:vec 1.0d0 2.0d0) (vec:vec 2.0d0 10.0d0))
           (vec:vec -0.12403473458920847d0 -0.9922778767136677d0))

  (do-test (vec:len2 (vec:vec 1.0d0 2.0d0)) 5)

  (do-test (vec:len (vec:vec 1.0d0 2.0d0)) 2.23606797749979d0)

  (do-test (vec:len (vec:vec 1.0d0 2.0d0)) 2.23606797749979d0)

  (do-test (vec:dst (vec:vec 1.0d0 2.0d0) (vec:vec 1.0d0 3.0d0)) 1.0d0)

  (do-test (vec:mid (vec:vec 1.0d0 2.0d0) (vec:vec 3.0d0 4.0d0))
           (vec:vec 2.0d0 3.0d0))

  (do-test (vec:lsum (list (vec:vec 1.0d0 2.0d0) (vec:vec 0.5d0 4.322d0)))
           (vec:vec 1.5d0 6.322d0))

  (do-test (vec:on-line 0.34d0 (vec:vec 33d0 88d0) (vec:vec 32d0 733d0))
           (vec:vec 32.66d0 307.3d0))

  (do-test (vec:on-circ 0.34d0 388d0 :xy (vec:vec 32d0 733d0))
           (vec:vec -175.9007964518508d0 1060.5992350947818d0))

  (do-test
    (vec:segdst (list (vec:vec 0d0 0d0) (vec:vec 100d0 0d0)) (vec:vec 0d0 200d0))
    200d0)

  (do-test
    (vec:segdst (list (vec:vec 0d0 0d0) (vec:vec 100d0 3d0)) (vec:vec 41d0 202d0))
    200.67971443818558d0)

  (do-test
    (multiple-value-bind (x s)
      (vec:segx (list (vec:rep 1.1d0) (vec:vec 11d0 12.3d0))
                (list (vec:vec 0.1d0 10d0) (vec:vec 8d0 -1.1d0)))
      (list x s))

    (list t 0.2984826334627212d0))

  (do-test
    (vec:segx (list (vec:vec 0d0 0d0) (vec:vec 100d0 0d0))
              (list (vec:vec 0d0 1d0) (vec:vec 100d0 1d0)))
    nil)

  (do-test
    (vec:segx (list (vec:vec 0d0 0d0) (vec:vec 1d0 1d0))
              (list (vec:vec 0d0 1d0) (vec:vec 1d0 0d0)))
    t)

  (do-test (vec:cross (vec:vec 1d0 2d0) (vec:vec 3d0 -7.1d0)) -13.1d0)


  (let ((lines `((#s(vec:vec :x 171.65283402050164d0 :y 440.93255770900925d0)
                  #s(vec:vec :x 741.8656230212274d0 :y 500.5731525084898d0))
                 (#s(vec:vec :x 419.2018008481497d0 :y 414.82076547119027d0)
                  #s(vec:vec :x 231.8129657641091d0 :y 432.9209704173471d0))
                 (#s(vec:vec :x 759.3100984542136d0 :y 683.0572688408785d0)
                  #s(vec:vec :x 186.78312653996636d0 :y 619.826955848491d0))
                 (#s(vec:vec :x 207.3487871315644d0 :y 423.4641291347193d0)
                  #s(vec:vec :x 572.5946942446438d0 :y 615.4467255272378d0))
                 (#s(vec:vec :x 711.763693083491d0 :y 758.0848939089993d0)
                  #s(vec:vec :x 459.2986854439291d0 :y 346.83449422454197d0))
                 (#s(vec:vec :x 265.3344135246598d0 :y 207.99967467598123d0)
                  #s(vec:vec :x 310.4842546275027d0 :y 335.82329421968967d0))
                 (#s(vec:vec :x 802.4973311106501d0 :y 409.70738975628706d0)
                  #s(vec:vec :x 434.1851185215588d0 :y 517.2401499837753d0))
                 (#s(vec:vec :x 434.1851185215588d0 :y 517.2401499837753d0)
                  #s(vec:vec :x 212.81495034634577d0 :y 595.0765635282548d0))
                 (#s(vec:vec :x 212.81495034634577d0 :y 595.0765635282548d0)
                  #s(vec:vec :x 534.7714916499588d0 :y 835.0868260231018d0))
                 (#s(vec:vec :x 534.7714916499588d0 :y 835.0868260231018d0)
                  #s(vec:vec :x 829.0282695332805d0 :y 661.0107297445226d0))
                 (#s(vec:vec :x 829.0282695332805d0 :y 661.0107297445226d0)
                  #s(vec:vec :x 303.64388839183994d0 :y 341.3925228044966d0))
                 (#s(vec:vec :x -250.5123166337458d0 :y -94.37721145333352d0)
                  #s(vec:vec :x -228.09104043643808d0 :y -83.28631005497442d0))
                 (#s(vec:vec :x -390.63856869263486d0 :y -18.767200281988693d0)
                  #s(vec:vec :x -362.24168246043087d0 :y -24.284515874982528d0))
                 (#s(vec:vec :x 43.8599536524796d0 :y 355.62019599604906d0)
                  #s(vec:vec :x 35.84958638173774d0 :y 315.5535786322419d0))
                 (#s(vec:vec :x -219.7604274076626d0 :y 68.60494549926393d0)
                  #s(vec:vec :x -205.85946598681753d0 :y 79.85598143394662d0))
                 (#s(vec:vec :x 61.768527387278645d0 :y -203.25895028554947d0)
                  #s(vec:vec :x 99.92784427171932d0 :y -172.27011340884923d0))
                 (#s(vec:vec :x -117.32216116994307d0 :y -3.7842875742150794d0)
                  #s(vec:vec :x -136.2033645477127d0 :y 2.7374756681442522d0)))))

    (do-test (vec:lsegx lines)
             #(((4 . 0.6473247799031681d0) (6 . 0.6764393351790597d0)
                (10 . 0.6260663447012615d0) (3 . 0.1509141161441d0))
                nil ((4 . 0.1753865343537019d0) (8 . 0.8785488586328714d0))
                ((7 . 0.5416053935281867d0) (0 . 0.13787206091662915d0))
                ((9 . 0.049416251337078505d0) (10 . 0.6536142074487687d0)
                 (6 . 0.6637385079514634d0) (2 . 0.2094037369836755d0)
                 (0 . 0.677313630605207d0)) nil
                ((4 . 0.7013190892911684d0) (10 . 0.7091860670441557d0)
                 (0 . 0.6655498482245016d0))
                ((3 . 0.131079893793497d0)) ((2 . 0.13511833062912754d0))
                ((4 . 0.5590910372417385d0))
                ((4 . 0.5372814694517455d0) (6 . 0.5476615564549062d0)
                 (0 . 0.5717421563781039d0)) nil nil nil nil nil nil))))


(defun %test-3vec ()

  (do-test (vec:3len (vec:3vec 1d0 1d0 0d0)) 1.4142135623730951d0)

  (do-test (vec:3dst (vec:3vec 1d0 1d0 0.5d0) (vec:3vec 0d0 2d0 1.5d0))
           1.7320508075688772d0)

  (do-test (vec:3cross (vec:3vec 0d0 1d0 0.5d0) (vec:3vec 0d0 2d0 1.5d0))
           (vec:3vec 0.5d0 0d0 0d0))

  (do-test (multiple-value-bind (x d p)
             (vec:3planex (vec:3vec 0d0 0d0 1d0) vec:*3zero*
                          (list (vec:3vec 1d0 1d0 -1d0)
                                (vec:3vec 1d0 1d0 1d0)))
             (list x d p))
           (list t 0.5d0 (vec:3vec 1.0d0 1.0d0 0.0d0)))

  (do-test (multiple-value-bind (x hits)
             (vec:3spherex (vec:3zero) 1d0 (list (vec:3rep 2d0) (vec:3rep -2d0)))
             (list x hits))
           (list t (list 0.6443375672974073d0 0.3556624327025929d0))))


(define-file-tests test-vec ()
  (test-title (%test-vec))
  (test-title (%test-3vec)))
