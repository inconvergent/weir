(load "~/quicklisp/setup.lisp")

; set the path to the folder containing weir.asd:
#+quicklisp (push "~/x/weir" ql:*local-project-directories*)

(ql:quickload :weir)

