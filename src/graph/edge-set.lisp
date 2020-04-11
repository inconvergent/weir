
(in-package :graph)

(defun -sort-edge (e)
  (sort e #'<))

(defun cycle->edge-set (cycle)
  (declare (list cycle))
  (loop for a in cycle and b in (cdr cycle)
        collect (-sort-edge (list a b))))


(defun edge-set->graph (es)
  (declare (list es))
  (loop with grph = (make)
        for (a b) in es do (add grph a b)
        finally (return grph)))


(defun -do-cycle-walk (grph visited cycle)
  (declare #.*opt-settings*
           (graph grph) (hash-table visited) (vector cycle))
  (let ((edges (get-incident-edges grph (vector-last cycle))))
    (when (not (= (length (the list edges)) 2))
          (return-from -do-cycle-walk nil))
    (loop named lp
          for v of-type pos-int being the hash-keys
            of (hset:make :init (alexandria:flatten edges))
          if (not (hset:mem visited v))
          do (vextend v cycle)
             (hset:add visited v)
             (return-from lp t))))

(defun -hash-table-first (ht)
  (declare (hash-table ht))
  (loop for k of-type pos-int being the hash-keys of ht
        repeat 1 return k))

(defun edge-set->cycle (es &aux (grph (edge-set->graph es)))
  (declare (list es) (graph grph))
  (with-struct (graph- adj) grph
    (loop with s = (-hash-table-first adj)
          with cycle = (make-adjustable-vector :init (list s) :type 'pos-int)
          with visited = (hset:make :init (list s))
          with n of-type pos-int = (hash-table-count adj)
          until (= (length cycle) n)
          if (not (-do-cycle-walk grph visited cycle))
          do (return-from edge-set->cycle nil)
          finally (return
            (if (not (= (hash-table-count visited) (length cycle)))
                ; TODO: don't print this
                (progn (print "edge-set->cycle warning: disjoint cycle.")
                       nil)
                (math:close-path (to-list cycle)))))))


(defun edge-set-symdiff (esa esb)
  (declare (list esa esb))
  (remove-if (lambda (e) (and (member e esa :test #'equal)
                              (member e esb :test #'equal)))
             (union esa esb :test #'equal)))


(defun cycle-basis->edge-sets (basis)
  (declare (list basis))
  (loop for c of-type list in basis collect (cycle->edge-set c)))

(defun edge-sets->cycle-basis (es)
  (declare (list es))
  (loop for e of-type list in es collect (edge-set->cycle e)))

(defun -edge-set-weight (es weightfx)
  (declare (list es) (function weightfx))
  (loop for e of-type list in es sumMing (apply weightfx e)))

(defun -sort-edge-sets (edge-sets weightfx)
  (declare (list edge-sets) (function weightfx))
  (mapcar #'second
    (sort (loop for es of-type list in edge-sets
                collect (list (-edge-set-weight es weightfx) es))
          #'> :key #'first)))

