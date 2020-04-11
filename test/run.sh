#!/bin/bash

set -e

for fn in `ls *lisp | grep -v '_'`;
  do
    echo $fn;
    ./$fn;
  done

