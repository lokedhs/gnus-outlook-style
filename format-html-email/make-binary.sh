#!/bin/sh

TMP=${TMP-/tmp}
SCRIPT_DIR=$(dirname $(readlink -f $0))

if wget -c 'http://beta.quicklisp.org/archive/closure-html/2014-08-26/closure-html-20140826-git.tgz' -O $TMP/closure-html-20140826-git.tgz
then

    rm -rf $TMP/closure-html-20140826-git
    tar xzf $TMP/closure-html-20140826-git.tgz -C $TMP

    cd $TMP/closure-html-20140826-git
    patch -p1 < $SCRIPT_DIR/../closure-html.diff
    cd -

    ls $TMP/closure-html-20140826-git

    sbcl --no-linedit <<EOF
(progn
  (require :asdf)
  (pushnew #p"$SCRIPT_DIR/" asdf:*central-registry* :test #'equal)
  (pushnew #p"$TMP/closure-html-20140826-git/" asdf:*central-registry* :test #'equal)
  (ql:quickload "format-html-email")
  (save-lisp-and-die "format_quoted_mail" :toplevel (find-symbol "MAIN" "FORMAT-HTML-EMAIL") :executable t))'
EOF

    rm -rf $TMP/closure-html-20140826-git $TMP/closure-html-20140826-git.tgz

fi
