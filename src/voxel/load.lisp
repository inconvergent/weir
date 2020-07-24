
(in-package :voxels)

(declaim (inline -trim))
(defun -trim (s)
  (declare (string s))
  (string-trim
    '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout) s))

(declaim (inline -split))
(defun -split (s)
  (declare (string s))
  (split-sequence:split-sequence #\Space s))

(defun file-load (fn dim &key (buffer-width 4000))
  (declare (string fn) (list dim))
  "
  load a file where each line is on the format:
  z1 y1 vx1 vx2 vx3 ...
  z1 y2 vx1 vx2 vx3 ...
  ...
  z2 y1 vx1 vx2 vx3 ...
  ...
  "

  (let ((voxs (voxels:make dim))
        (maxv 0))
    (declare (voxels voxs) (pos-int maxv))
    (dat::do-lines fn
      (lambda (row*)
        (let* ((row (mapcar #'parse-integer (-split (-trim row*))))
               (z (pop row))
               (y (pop row)))
          (declare (list row) (fixnum z y))
          (loop for v of-type pos-int in row
                and x of-type pos-int from 0
                do (voxels:setvoxel voxs x y z v)
                   (when (> v maxv) (setf maxv v)))))
      :buffer-width buffer-width)
    (values voxs maxv)))
