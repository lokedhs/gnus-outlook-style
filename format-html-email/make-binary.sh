#!/bin/sh

sbcl --eval '(progn (ql:quickload "format-html-email") (save-lisp-and-die "format_quoted_mail" :toplevel (find-symbol "MAIN" "FORMAT-HTML-EMAIL") :executable t))'
