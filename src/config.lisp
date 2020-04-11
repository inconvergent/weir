;;; These are configuration settings for the project.
;;;
;;; These settings aren't particularly friendly to projects more
;;; broadly as they're not "self contained."

(in-package #:weir)

(setf *random-state* (make-random-state t))
(setf *print-pretty* t)
