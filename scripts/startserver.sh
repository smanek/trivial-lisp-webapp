#!/bin/sh
cd `dirname $0` #cd into the /scripts/ directory
sbcl --no-sysinit --no-userinit --load ../src/init.lisp