#!/bin/bash

sbcl --quit\
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(load "weir.asd")'\
     --eval '(ql:quickload :weir)'\
     --eval '(asdf:test-system :weir)'
