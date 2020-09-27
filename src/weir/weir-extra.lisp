(in-package :weir)


(defun cut-to-area! (wer w h &key g)
  "
  removes all edges (in g) outside envelope (0d0 0d0), (w h).
  all edges intersecting the envelope will be deleted, a new vert will be
  inserted on the intersection. connected to the inside vert.
  edges inside the envelope will be left as they are.
  "
  (declare (weir wer) (double-float w h))
  (labels
    ((inside (pt)
      (declare (vec:vec pt))
      (vec:with-xy (pt x y) (and (>= x 0d0) (>= y 0d0) (<= x w) (<= y h))))

     (split-line (line &aux (rev nil))
       (declare (list line) (boolean rev))
       (unless (inside (first line)) (setf line (reverse line) rev t))
       (destructuring-bind (a b) line
         (declare (vec:vec a b))
         (return-from split-line
           (vec:with-xy (a xa ya)
             (vec:with-xy (b xb yb)
               (list rev (vec:lon-line
                           (cond ((> xb w) (/ (- w xa) (- xb xa)))
                                 ((> yb h) (/ (- h ya) (- yb ya)))
                                 ((< xb 0d0) (/ (- xa) (- xb xa)))
                                 ((< yb 0d0) (/ (- ya) (- yb ya))))
                           line)))))))

     (cutfx (line)
       (declare (list line))
       (let ((c (length (remove-if-not (lambda (v) (inside v)) line))))
         (declare (fixnum c))
         (cond ((= c 0) (values :none nil (vec:zero)))
               ((= c 1) (destructuring-bind (rev pt) (split-line line)
                          (values :split rev pt)))
               (t (values :keep nil (vec:zero)))))))

    (with (wer %)
      (itr-edges (wer e :g g)
        (multiple-value-bind (state rev pt) (cutfx (get-verts wer e))
          (declare (symbol state) (boolean rev) (vec:vec pt))
          (cond ((equal state :keep) t)
                ((equal state :none) (% (ldel-edge? e :g g)))
                ((equal state :split)
                   (% (ldel-edge? e :g g))
                   (% (append-edge?
                        (if rev (second e) (first e)) pt :rel nil :g g)))))))))

