
(in-package :mesh)


(defun obj-load (msh fn)
  (declare (mesh msh))
  (labels
    ((add-poly (o) (mesh:add-polygon! msh (mapcar (lambda (i) (1- i)) o)))
     (add-vert (o) (mesh:add-vert! msh (vec:3vec* o)))
     (parse (l) (cond ((eq (first l) 'cl-user::f) (add-poly (cdr l)))
                      ((eq (first l) 'cl-user::v) (add-vert (cdr l)))))
     (do-line (l)
       (let ((*read-default-float-format* 'double-float))
         (parse (loop for x = (read l nil nil)
                      for i of-type pos-int from 0
                      while x collect x)))))
    (dat:do-lines-as-buffer fn #'do-line))
  msh)

