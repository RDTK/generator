#!/bin/sh

sbcl \
     --noinform --no-userinit --disable-debugger            \
     --load ~/.local/share/common-lisp/quicklisp/setup.lisp \
     --load dump.lisp                                       \
     --quit
