
(in-package :graph)


; STRIP FILAMENTS

(defun -del-filament (grph v)
  (declare (graph grph) (pos-int v))
  (let ((ee (get-incident-edges grph v)))
    (when (= (length ee) 1)
          (apply #'del grph (first ee)))))

(defun del-simple-filaments (grph)
  (declare (graph grph))
  "
  recursively remove all simple filament edges until there are none left
  "
  (loop until (notany #'identity
                      (loop for v in (hset:to-list (graph-verts grph))
                            collect (-del-filament grph v))))
  grph)

; CONTINOUS PATHS

;note: this can possibly be improved if k is an array
(defun -cycle-info (k)
  (declare (list k))
  (if (= (first k) (first (last k)))
      (list (cdr k) t)
      (list k nil)))

(defun -find-continous (grph start curr)
  (declare (graph grph) (pos-int start curr))
  (loop with res = (make-adjustable-vector :type 'pos-int :init (list start))
        with prev of-type pos-int = start
        while t
        do (let* ((incident (get-incident-edges grph curr))
                  (n (length incident)))
             (declare (pos-int n))

             ; loop. attach curr to indicate loop
             (when (= curr start)
                   (vextend curr res)
                   (return-from -find-continous res))

             ; dead end/multi
             (unless (= n 2)
                     (vextend curr res)
                     (return-from -find-continous res))

             ; single connection
             (when (= n 2)
                   (let ((c (remove-if (lambda (i) (= i curr))
                                       (-only-incident-verts prev incident))))
                     (vextend curr res)
                     (setf prev curr curr (first c)))))))

(defun -add-visited-verts (visited path)
  (loop for v in path do (setf (gethash v visited) t)))

(defun get-continous-paths (grph &key cycle-info)
  (declare (graph grph))
  "
  greedily finds connected paths between multi-intersection points.
  TODO: rewrite this to avoid cheching everything multiple times.
  i'm sorry.
  "
  (let ((all-paths (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'equal)))

    (labels
      ((-incident-not-two (incident)
         (declare (list incident))
         (not (= (length incident) 2)))

       (-incident-two (incident)
         (declare (list incident))
         (= (length incident) 2))

       (-do-find-continous (v next)
         (declare (pos-int v next))
         (let* ((path (to-list (-find-continous grph v next)))
                (key (sort (copy-list path) #'<)))
           (declare (list path key))
           (unless (gethash key all-paths)
                   (-add-visited-verts visited path)
                   (setf (gethash key all-paths) path))))

       (-walk-incident-verts (v testfx)
         (declare (pos-int v) (function testfx))
         (let ((incident (get-incident-edges grph v)))
           (declare (list incident))
           (when (funcall testfx incident)
                 (loop for next in (-only-incident-verts v incident)
                       do (-do-find-continous v next))))))

      (loop for v in (sort (get-verts grph) #'<)
            do (-walk-incident-verts v #'-incident-not-two))

      ; note: this can be improved if we inverted visited, and remove vertices
      ; as they are visited
      (loop for v in (sort (get-verts grph) #'<)
            do (when (not (gethash v visited))
                     (-walk-incident-verts v #'-incident-two))))

    (loop for k of-type list being the hash-values of all-paths
          if cycle-info collect (-cycle-info k) of-type list
          else collect k of-type list)))

