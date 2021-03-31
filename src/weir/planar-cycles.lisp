
(in-package :weir)

(defun -get-west-most-vert (verts vertfx incidentfx)
  (declare (list verts) (function vertfx incidentfx))
  (unless verts (return-from -get-west-most-vert nil))
  (let* ((res (car verts))
         (v (funcall vertfx res))
         (mx (vec:vec-x v))
         (my (vec:vec-y v)))
    (declare (fixnum res) (double-float mx my))
    (loop for i in (cdr verts)
          do (when (> (length (funcall incidentfx i)) 0)
                   (vec:with-xy ((funcall vertfx i) x y)
                     (when (or (< x mx) (and (<= x mx) (< y my)))
                           (setf res i mx x my y)))))
    res))

(defun get-west-most-vert (wer &key g)
  (declare (weir wer))
  "
  get west-most vert with at least one incident vert.
  if there are multple candidates, select the one with lowest y value.
  "
  (-get-west-most-vert (get-connected-verts wer :g g)
                       (lambda (v) (get-vert wer v))
                       (lambda (v) (get-incident-edges wer v))))


(defun -dot-perp (a b)
  (declare (vec:vec a b))
  (vec:dot a (vec:perp b)))

(defun -is-convex (a b)
  (declare (vec:vec a b))
  (<= (-dot-perp a b) 0d0))

(defun -get-cw-most-vert (curr prev &key dirfx adjfx)
  (declare (fixnum curr prev) (function dirfx adjfx))
  (let ((dcurr (if (< prev 0) (vec:vec 0d0 -1d0) (funcall dirfx curr prev)))
        (adj (funcall adjfx curr prev)))

    (if (not adj) (return-from -get-cw-most-vert nil))

    (loop with next of-type fixnum = (car adj)
          with dnext of-type vec:vec = (funcall dirfx next curr)
          with convex of-type boolean = (-is-convex dnext dcurr)
          for a of-type fixnum in (cdr adj)
          do (let ((da (funcall dirfx a curr)))
               (if convex (when (or (< (-dot-perp dcurr da) 0d0)
                                    (< (-dot-perp dnext da) 0d0))
                                (setf next a
                                      dnext da
                                      convex (-is-convex dnext dcurr)))
                          (when (and (< (-dot-perp dcurr da) 0d0)
                                     (< (-dot-perp dnext da) 0d0))
                                (setf next a
                                      dnext da
                                      convex (-is-convex dnext dcurr)))))
          finally (return next))))

(defun -get-ccw-most-vert (curr prev &key dirfx adjfx)
  (declare (fixnum curr prev) (function dirfx adjfx))
  (let ((dcurr (if (< prev 0) (vec:vec 0d0 -1d0) (funcall dirfx curr prev)))
        (adj (funcall adjfx curr prev)))
    (if (not adj) (return-from -get-ccw-most-vert nil))
    (loop with next of-type fixnum = (car adj)
          with dnext of-type vec:vec = (funcall dirfx next curr)
          with convex of-type boolean = (-is-convex dnext dcurr)
          for a of-type fixnum in (cdr adj)
          do (let ((da (funcall dirfx a curr)))
               (if convex (when (and (> (-dot-perp dcurr da) 0d0)
                                     (> (-dot-perp dnext da) 0d0))
                                (setf next a
                                      dnext da
                                      convex (-is-convex dnext dcurr)))
                          (when (or (> (-dot-perp dcurr da) 0d0)
                                    (> (-dot-perp dnext da) 0d0))
                                (setf next a
                                      dnext da
                                      convex (-is-convex dnext dcurr)))))
          finally (return next))))

(defun get-incident-rotated-vert (wer curr &key (dir :ccw) (prev -1) g)
  (declare (weir wer) (symbol dir) (fixnum curr prev))
  "
  (counter-)clockwise-most vertex of curr, relative to prev (or (0 -1))
  NOTE: assuming the up is positive y, and right is positive x.
  TODO: adjust this to handle that positive y is down? does it matter?
  "
  (labels ((dirfx (a b) (apply #'vec:sub (weir:get-verts wer (list a b))))
           (adjfx (c p) (remove-if (lambda (i) (= i p))
                                   (graph::-only-incident-verts c
                                     (get-incident-edges wer c :g g)))))
    (case dir
          (:cw (-get-cw-most-vert curr prev :dirfx #'dirfx :adjfx #'adjfx))
          (:ccw (-get-ccw-most-vert curr prev :dirfx #'dirfx :adjfx #'adjfx))
          (t (error "dir must be :cw or :ccw")))))


(defun -do-walk-cycle (grph &key dirfx adjfx vertfx)
  (declare (graph::graph grph) (function dirfx adjfx))
  (when (< (graph:get-num-edges grph) 1) (return-from -do-walk-cycle nil))
  (let* ((prev (-get-west-most-vert
                 (graph:get-verts grph)
                 vertfx
                 (lambda (v) (graph:get-incident-edges grph v))))
         (start prev)
         (next (-get-cw-most-vert prev -1 :dirfx dirfx :adjfx adjfx))
         (res (list next prev)))

    (unless next (return-from -do-walk-cycle nil))

    (loop until (= next start)
          do (let ((c (-get-ccw-most-vert next prev :dirfx dirfx :adjfx adjfx)))
               (push c res)
               (setf prev next next c)))
    (setf res (reverse res))

    (graph:del grph (first res) (second res))
    (graph:del-simple-filaments grph)

    ; return nil if it is a non-cycle closed walk
    (if (> (length res) (+ (length (remove-duplicates res)) 1)) nil res)))

(defun get-planar-cycles (wer &key cycle-info g &aux (res (list)))
  (declare (weir wer) (list res))
  (multiple-value-bind (g* exists) (gethash g (weir-grps wer))
    (unless exists (error "attempted to access invalid group: ~a" g))
    (let ((grph (graph:copy (grp-grph g*))))
      (graph:del-simple-filaments grph)
      (labels
        ((vertfx (v) (get-vert wer v))
         (dirfx (a b) (apply #'vec:sub (weir:get-verts wer (list a b))))
         (adjfx (c p) (remove-if (lambda (i) (= i p))
                                 (graph::-only-incident-verts c
                                   (graph:get-incident-edges grph c)))))

        (loop while (> (graph:get-num-edges grph) 0)
              do (let ((cycle (-do-walk-cycle grph :dirfx #'dirfx
                                :adjfx #'adjfx :vertfx #'vertfx)))
                   (when cycle (push cycle res)))))))

  (if cycle-info (loop for c in res collect (graph::-cycle-info c) of-type list)
                 res))

