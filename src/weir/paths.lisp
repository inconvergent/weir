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

(defun get-segments (wer &key cycle-info g)
  (with-grp (wer grp g)
    (graph:get-segments (grp-grph grp) :cycle-info cycle-info)))

(defun walk-graph (wer &key g)
  (labels ((-angle (a b c)
             (destructuring-bind (va vb vc) (gvs wer (list a b c))
               (vec:dot (vec:norm (vec:sub vb va))
                        (vec:norm (vec:sub vc vb))))))
    (with-grp (wer grp g)
      (graph:walk-graph (grp-grph grp) :angle #'-angle))))


; INTERSECTS

(deftype array-list () `(simple-array list))

(defun intersect-all! (wer &key g)
  (declare #.*opt-settings* (weir wer))
  (-dimtest wer)

  (let ((crossing->vert (make-hash-table :test #'equal)))
    (declare (hash-table crossing->vert))

    (labels
      ((-ic (i c) (declare (fixnum i c)) (if (< i c) (list i c) (list c i)))

       (-add (line i hits)
         (declare (list line hits) (fixnum i))
         (loop for (c . p) in hits
               if (not (gethash (the list (-ic i c)) crossing->vert))
               do (setf (gethash (the list (-ic i c)) crossing->vert)
                        (add-vert! wer (vec:lon-line p line)))))

       (-add-new-verts (edges isects)
         (declare (array-list edges isects))
         (loop for hits across isects
               for i of-type fixnum from 0
               if hits
               do (-add (gvs wer (aref edges i)) i hits)))

       (-edges-as-lines (edges)
         (declare (array-list edges))
         (loop for edge of-type list across edges
               collect (gvs wer edge)))

       (-del-hit-edges (edges isects g)
         (declare (array-list edges isects))
         (loop for hits of-type list across isects
               for i of-type fixnum from 0
               if hits
               do (ldel-edge! wer (aref edges i) :g g)
                  (loop for (c . p) in hits
                        do (ldel-edge! wer (aref edges c) :g g))))

       (-sort-hits (isects)
         (loop for i of-type fixnum from 0 below (length isects)
               if (aref isects i)
               do (setf (aref isects i) (sort (aref isects i) #'< :key #'cdr)))
         isects)

       (-add-new-edges (edges isects g)
         (declare (array-list edges isects))
         (loop for hits of-type list across isects
               for i of-type fixnum from 0
               if hits
               do (loop with cc = (math:lpos hits)
                        for a of-type fixnum in cc
                        and b of-type fixnum in (cdr cc)
                        initially
                          (add-edge! wer
                            (gethash (-ic i (first cc)) crossing->vert)
                            (first (aref edges i)) :g g)
                          (add-edge! wer
                            (gethash (-ic i (last* cc)) crossing->vert)
                            (last* (aref edges i)) :g g)
                        do (add-edge! wer
                             (gethash (-ic i a) crossing->vert)
                             (gethash (-ic i b) crossing->vert) :g g)))))

      (let* ((edges (to-vector (get-edges wer :g g)))
             (lines (to-vector (-edges-as-lines edges)))
             (isects (-sort-hits (vec:lsegx lines))))
        (declare (array-list isects edges lines))
        (-del-hit-edges edges isects g)
        (-add-new-verts edges isects)
        (-add-new-edges edges isects g))))
  nil)


