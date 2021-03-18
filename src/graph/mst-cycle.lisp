
(in-package :graph)

(defvar *inf* 1d8)


; SPANNING TREE

(defun -do-spanning-tree (grph st visited start)
  (declare (graph grph st) (hash-table visited) (fixnum start))
  (loop with curr of-type fixnum = -1
        with stack of-type list = (list (list start start))
        while stack
        do (destructuring-bind (curr* parent) (pop stack)
             (declare (fixnum curr* parent))
             (setf curr curr*)
             ; if not visited, visit, and add descendants
             (when (hset:add visited curr)
                   (unless (= curr parent) (graph:add st curr parent))
                   (loop for next of-type fixnum
                           in (get-incident-verts grph curr)
                         do ; add (next curr) to stack
                            (setf stack (cons (list next curr) stack))))))
  st)

(defun get-spanning-tree (grph &key start)
  "
  return all spanning trees (if the graph is disjoint) of grph in a new graph.
  if start is provided, it will return a spanning tree starting at start.
  "
  (declare (graph grph))
  (let ((visited (hset:make))
        (st (make))
        (num 0))
    (declare (hash-table visited) (graph st) (fixnum num))
    (if start (progn (-do-spanning-tree grph st visited start)
                     (setf num 1))
              (loop for v of-type fixnum in (get-verts grph)
                    unless (hset:mem visited v)
                    do (-do-spanning-tree grph st visited v)
                       (incf num)))
    ; num is the number of subgraphs/trees
    (values st num)))


; MIN SPANNING TREE

(defun -do-min-spanning-tree (grph q weight edge weightfx
                              &aux (c 0d0) (mst (make)))
  (declare (graph grph mst) (hash-table weight edge q)
           (function weightfx) (double-float c))
  ; some version of prim's algorithm.
  ; missing binary heap to find next vertex to add
  (labels
    ((-find-next-min-edge ()
       (loop with cv of-type double-float = (+ 1d0 *inf*) ; cost
             with v of-type fixnum = -1 ; index
             for w of-type fixnum being the hash-keys of q
             do (when (< (gethash w weight) cv)
                      ; update minimum: v=w and cv=weight[w]
                      (setf v w cv (gethash w weight)))
             finally (return (values v cv))))

     (-update-descendant (v w cw)
       (multiple-value-bind (_ exists) (gethash w q)
         (declare (ignore _) (boolean exists))
         (when (and exists (< cw (gethash w weight)))
               ; update: weight[w]=cw and edge[w]=v
               (setf (gethash w weight) cw (gethash w edge) v)))))

    (loop while (> (hash-table-count q) 0)
          do (multiple-value-bind (v c*) (-find-next-min-edge)
               (declare (fixnum v) (double-float c*))
               (when (< v 0) (error "mst: hit negative vert"))
               ; remove (min) v from q
               (remhash v q)
               ; when edge exists
               (multiple-value-bind (w exists) (gethash v edge)
                 ; add edge to graph
                 (when (and exists w) (add mst v w) (incf c c*)))
               ; descendants of v
               (loop for w of-type fixnum in (get-incident-verts grph v)
                     do (-update-descendant v w (funcall weightfx v w))))))
  (values mst c))

(defun -init-hash-table (ht v)
  (declare (hash-table ht))
  (loop for k of-type fixnum being the hash-keys of ht
        do (setf (gethash k ht) v))
  ht)

(defun get-min-spanning-tree (grph &key (weightfx (lambda (a b)
                                                    (declare (ignore a b)) 1d0))
                                        (start 0))
  "
  return all minimal spanning trees of grph in a new graph.
  if start is provided, it will return a spanning tree starting at start.
  "
  ; TODO: what happens if grph is disjoint?
  (declare (graph grph) (fixnum start) (function weightfx))
  (let* ((verts (graph-verts grph))
         (weight (-init-hash-table (hset:copy verts) *inf*)) ; ht
         (edge (-init-hash-table (hset:copy verts) nil)) ; ht
         (q (hset:copy verts))) ; hset: verts not in tree
    (when start (setf (gethash start weight) 0d0))
    (-do-min-spanning-tree grph q weight edge weightfx)))


; CYCLE BASIS (this is pretty bad ...)

(defun -reduce-cycle (edge-sets i weightfx)
  (declare (simple-array edge-sets) (fixnum i) (function weightfx))
  (labels
    ((-intersect (curr j)
       (intersection curr (aref edge-sets j) :test #'equal))

     (-smaller (curr sd)
       (multiple-value-bind (_ cycle) (edge-set->path sd)
         (declare (ignore _))
         (and cycle (< (-edge-set-weight sd weightfx)
                       (-edge-set-weight curr weightfx)))))

     (-search-and-reduce (i)
       (loop with curr = (aref edge-sets i)
             for j from (1+ i) below (length edge-sets)
             if (-intersect curr j)
             do (let ((sd (edge-set-symdiff curr (aref edge-sets j))))
                  (when (-smaller curr sd)
                        (setf (aref edge-sets i) sd curr sd)
                        (return-from -search-and-reduce nil)))
             finally (return t))))

    (loop with done = nil
          while (not done)
          do (setf done (-search-and-reduce i)))))

(defun -reduce-cycle-basis (basis weightfx)
  (declare (list basis) (function weightfx))
  (loop with edge-sets =
          (to-vector (-sort-edge-sets (cycle-basis->edge-sets basis) weightfx))
        for es across edge-sets
        for i from 0 below (length edge-sets)
        do (-reduce-cycle edge-sets i weightfx)
        finally (return (edge-sets->cycle-basis (to-list edge-sets)))))


(defun -get-basis-cycle (st start stop)
  (declare (graph st) (fixnum start stop))
  (loop with curr of-type fixnum = -1
        with cycle = nil
        with visited of-type hash-table = (hset:make)
        with stack of-type list = (list (list start (list start stop)))
        until (= curr stop)
        do (destructuring-bind (curr* cycle*) (pop stack)
             (setf curr curr* cycle cycle*)
             (when (hset:add visited curr)
                   (loop for next of-type fixnum
                           in (get-incident-verts st curr)
                         do (setf stack (cons (list next (cons next cycle))
                                              stack)))))
        finally (return cycle)))

(defun get-cycle-basis (grph &key (weightfx (lambda (a b)
                                              (declare (ignore a b)) 1d0))
                             &aux (res (list)))
  (declare (graph grph) (function weightfx))
  (let ((st (get-min-spanning-tree grph :weightfx weightfx)))
    (with-graph-edges (grph e)
      (unless (apply #'mem st e)
              (let ((basis (apply #'-get-basis-cycle st e)))
                   (when basis (setf res (cons basis res)))))))
  (-reduce-cycle-basis res weightfx))

