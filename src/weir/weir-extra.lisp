(in-package :weir)


(defun cut-to-area! (wer &key top left bottom right g)
  "
  removes all edges (in g) outside envelope (ox oy), (w h).
  all edges intersecting the envelope will be deleted, a new vert will be
  inserted on the intersection. connected to the inside vert.
  edges inside the envelope will be left as they are.
  "
  (declare (weir wer) (double-float top left bottom right))
  (labels
    ((inside (pt)
      (declare (vec:vec pt))
      (vec:with-xy (pt x y)
        (and (> x left) (> y top) (< x right) (< y bottom))))

     (split-line (line &aux (rev nil))
       (declare (list line) (boolean rev))
       (unless (inside (first line)) (setf line (reverse line) rev t))
       (destructuring-bind (a b) line
         (declare (vec:vec a b))
         (return-from split-line
           (vec:with-xy (a xa ya)
             (vec:with-xy (b xb yb)
               (list rev (vec:lon-line
                           (cond ((> xb right) (/ (- right xa) (- xb xa)))
                                 ((> yb bottom) (/ (- bottom ya) (- yb ya)))
                                 ((< xb left) (/ (- left xa) (- xb xa)))
                                 (t (/ (- top ya) (- yb ya)))) ; (< yb top)
                           line)))))))

     (cutfx (line)
       (declare (list line))
       (let ((c (length (remove-if-not #'inside line))))
         (declare (fixnum c))
         (case c (0 (values :none nil (vec:zero)))
                 (1 (destructuring-bind (rev pt) (split-line line)
                      (values :split rev pt)))
                 (t (values :keep nil (vec:zero)))))))

    (with (wer %)
      (itr-edges (wer e :g g)
        (alexandria:with-gensyms (ae)
          (multiple-value-bind (state rev pt) (cutfx (get-verts wer e))
            (declare (symbol state) (boolean rev) (vec:vec pt))
            (case state (:none (% (ldel-edge? e :g g)))
                        (:split
                           (% (ldel-edge? e :g g))
                           (% (append-edge?
                                (if rev (second e) (first e)) pt :rel nil :g g)
                              :res ae)
                           (% (set-vert-prop? ae :cut) :arg (ae))))))))))

