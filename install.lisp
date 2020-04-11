#!/usr/bin/sbcl --script

(defvar *opt-settings* '(optimize (safety 2) (speed 3) (debug 3)))

(load "~/quicklisp/setup.lisp")

(ql:quickload "alexandria")
(ql:quickload "cl-svg")
(ql:quickload "png")
(ql:quickload "inferior-shell")
(ql:quickload "zpng")
(ql:quickload "parse-float")
(ql:quickload "cl-json")
(ql:quickload "lparallel")

