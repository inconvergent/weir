
(in-package :obj)


(defstruct obj
  (verts nil :type vector :read-only t)
  (faces nil :type vector :read-only t)
  (lines nil :type vector :read-only t)
  (num-verts 0 :type fixnum :read-only nil)
  (num-lines 0 :type fixnum :read-only nil)
  (num-faces 0 :type fixnum :read-only nil))


(defun make ()
  (make-obj :verts (make-adjustable-vector)
            :lines (make-adjustable-vector)
            :faces (make-adjustable-vector)))


(defun add-verts (o new)
  (declare (obj o) (list new))
  (with-struct (obj- verts) o
    (setf (obj-num-verts o) (incf (obj-num-verts o) (length new)))
    (loop with n = (length verts)
          for v of-type vec:3vec in new and i from n
          do (vextend v verts)
          collect i)))


(defun add-face (o new)
  (declare (obj o) (list new))
  (with-struct (obj- faces) o
    (setf (obj-num-faces o) (incf (obj-num-faces o)))
    (vextend new faces)))


(defun add-line (o new)
  (declare (obj o) (list new))
  (with-struct (obj- lines) o
    (setf (obj-num-lines o) (incf (obj-num-lines o)))
    (vextend new lines)))


(defun save (o fn &key (mesh-name "mesh"))
  (declare (obj o))
  (with-struct (obj- verts faces lines) o
    (with-open-file (fstream (ensure-filename fn ".obj")
                             :direction :output :if-exists :supersede)
      (declare (stream fstream))
      (format fstream "o ~a~%" mesh-name)
      (loop for v of-type vec:3vec across verts
            do (vec:3with-xy (v x y z)
                 (format fstream "v ~f ~f ~f~%" x y z)))
      (loop for ee of-type list across faces
            do (destructuring-bind (a b c) (math:add ee '(1 1 1))
                 (declare (fixnum a b c))
                 (format fstream "f ~d ~d ~d~%" a b c)))
      (loop for ll of-type list across lines
            do (format fstream "l")
               (loop for l of-type fixnum in ll
                     do (format fstream " ~d" (1+ l)))
               (format fstream "~%")))))

