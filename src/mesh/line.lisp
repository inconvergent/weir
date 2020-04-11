
(in-package :mesh)


(defun make-unhidden-line-fx (&key projectfx rayfx raycastfx (rs 1.5d0))
  (declare (function raycastfx projectfx rayfx) (double-float rs))
  (labels
    ((line-steps (line2)
       (declare (list line2))
       (ceiling (* rs (apply #'vec:dst line2))))

     (do-hidden-test (line)
       (declare (list line))
       (let ((line2 (funcall projectfx line)))
         (loop for s in (math:linspace (line-steps line2) 0d0 1d0)
               collect (list (atom (funcall raycastfx
                                     (funcall rayfx (vec:3lon-line s line))))
                             (vec:lon-line s line2)))))
     (first-and-last (f)
       (when (math:list>than f 1) (cons (first f) (last f))))

     (pts->lines (pts)
       (declare (list pts))
       (remove-if-not #'identity (mapcar #'first-and-last pts)))

     (line-split (res a)
       (declare (list res a))
       (if (first a)
           ; include point
           (cons (cons (second a) (first res)) (cdr res))
           ; else, drop point, if (first res) is not nil. append nil,
           ; otherwise return res
           (if (first res) (cons (list) res) res)))

     (split-non-hidden (pts)
       (declare (list pts))
       (reduce #'line-split pts :initial-value (list (list))))

     (do-line (line)
       (declare (list line))
       (pts->lines (split-non-hidden (do-hidden-test line)))))

    #'do-line))

