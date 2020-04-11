
(in-package :mesh)


(defun make-cone-light (&key xy (rad 0.6d0) (look vec:*3zero*) (len 5000d0))
  (declare #.*opt-settings* (vec:3vec xy look) (double-float rad len))
  "
  larger rad is wider cone
  "
  (let ((dir (vec:3norm (vec:3sub look xy))))
    (declare (vec:3vec dir))
    (lambda ()
      (loop with res of-type vec:3vec = (rnd:3in-sphere :rad rad :xy dir)
            until (> (vec:3dot res dir) 0d0)
            do (setf res (rnd:3in-sphere :rad rad :xy dir))
            finally (return (list xy (vec:3from xy (vec:3norm! res) len)))))))

