#!/bin/bash

set -e
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(load "weir.asd")'\
     --eval '(handler-case (ql:quickload :weir :verbose nil)
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
     --eval '(asdf:test-system :weir)' | tee run-tests.sh.tmp

