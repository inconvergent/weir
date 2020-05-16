(in-package :weir)

; SPANNING TREE

(defun get-spanning-tree (wer &key g edges start)
  (with-grp (wer grp g)
    (if edges (graph:get-edges
                (graph:get-spanning-tree (grp-grph grp) :start start))
              (graph:get-spanning-tree (grp-grph grp) :start start))))


(defun get-min-spanning-tree (wer &key g edges start)
  (with-grp (wer grp g)
    (let ((grph (grp-grph grp))
          (weigthfx (lambda (a b) (weir:edge-length wer a b))))
      (if edges (graph:get-edges
                  (graph:get-min-spanning-tree grph
                    :start start :weightfx weigthfx))
                (graph:get-min-spanning-tree grph
                   :start start :weightfx weigthfx)))))


; CYCLE BASIS

(defun get-cycle-basis (wer &key g)
  (let ((weightfx (if (= (weir-dim wer) 2)
                      (lambda (a b) (weir:edge-length wer a b))
                      (lambda (a b) (weir:3edge-length wer a b)))))
    (declare (function weightfx))
    (with-grp (wer grp g)
      (graph:get-cycle-basis (grp-grph grp) :weightfx weightfx))))

; CONTINOUS PATHS

(defun get-continous-paths (wer &key cycle-info g)
  (with-grp (wer grp g)
    (graph:get-continous-paths (grp-grph grp) :cycle-info cycle-info)))


; INTERSECTS

(defun -do-intersects (wer split-edge &key g (tol 0.000001d0)
                                      &aux (line (get-verts wer split-edge)))
  (declare (weir wer) (list split-edge line))
  (itr-edges (wer e :g g)
    (when (< (length (intersection split-edge e)) 1)
          (multiple-value-bind (x s) (vec:segx line (get-verts wer e))
            (when (and x (< tol s (- 1d0 tol)))
                  (multiple-value-bind (anv ane)
                    (lsplit-edge! wer e :xy (vec:lon-line s line) :g g)
                    (return-from -do-intersects
                      (concatenate 'list ane
                        (lsplit-edge-ind! wer split-edge :via anv :g g)))))))))

(defun intersect-all! (wer &key g)
  (declare (weir wer))
  "
  inserts new vertices wherever any two lines intersect.
  "
  (-dimtest wer)
  (loop with edges = (get-edges wer :g g)
        with edge = (pop edges)
        while edge
        do (when (and edge (edge-exists wer edge :g g))
                 (let ((res (-do-intersects wer edge)))
                   (when res (setf edges (concatenate 'list edges res)))))
           (setf edge (pop edges))))

