
(in-package :mesh)


(defun obj-load (msh fn)
  (declare #.*opt-settings* (mesh msh))
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

(defun obj-export (msh fn &key (mesh-name "mesh"))
  (declare #.*opt-settings* (mesh msh) (string fn mesh-name))
  (with-open-file (fstream (ensure-filename fn ".obj")
                             :direction :output :if-exists :supersede)
    (declare (stream fstream))
    (format fstream "o ~a~%" mesh-name)
    (loop with verts of-type (simple-array double-float) = (mesh-verts msh)
          for i of-type fixnum from 0 below (* 3 (mesh-num-verts msh)) by 3
          do (format fstream "v ~f ~f ~f~%" (aref verts i)
                                            (aref verts (1+ i))
                                            (aref verts (+ i 2))))

    (loop for tri of-type list being the hash-keys of (mesh-polygons msh)
          do (destructuring-bind (a b c) (math:add tri '(1 1 1))
               (declare (fixnum a b c))
               (format fstream "f ~d ~d ~d~%" a b c)))))

