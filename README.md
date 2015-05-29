Outlook-style quoting for Gnus and mu4e
=======================================

Author contact information
--------------------------

  - Elias Martenson
  - Email: lokedhs@gmail.com

Summary
-------

There is a significant number of people who wants to use Emacs
for managing their email but work in organsisations that are using
Outlook. One major feature (or, misfeature perhaps) is the way
Outlook handles quoting. It includes the entire email chain at the
bottom of the mail, with the reply on top. Users of Outlook also
expects this chain to preserve all HTML formatting as well as any
inlined images.

This package allows Emacs users to participate in such threads. It
allows the user to write markup which is processed by Muse, and the
content is then inserted into the existing HTML, resulting in emails
blends perfectly into an Outlook thread.

Most of the work is performed by an external program written in
Common Lisp. Building this program is the most complicated part
of setting up this package. The section below will explain how
to do this.

Installing
==========

In emacs you will need `muse`. Open `M-x package-list-packages` and install it.

Install required packages:

    sudo apt-get install sbcl libfixposix-dev

Install Quicklisp:

    curl -O http://beta.quicklisp.org/quicklisp.lisp
    sbcl --load quicklisp.lisp

In the `sbcl` console:

    (quicklisp-quickstart:install)
    (ql:add-to-init-file)
    (quit)

Remove `quicklisp.lisp`, not needed any more.

Add to the end of `~/.sbclrc`, edit the path to your setup:

    (require :asdf)
    (pushnew "/path/to/gnus-outlook-style/format-html-email/" asdf:*central-registry* :test #'equal)

Now you are ready to start compiling the `format_quoted_email` helper binary:

    git clone https://github.com/lokedhs/gnus-outlook-style
    cd gnus-outlook-style/format-html-email/
    ./make_binary.sh

You should be seeing downloading packages, compilation messages, and a fail message for `format-html-email`. Time to apply the patch to `closure-html`.

Go to the `closure-html` folder:

    cd ~/quicklisp/dists/quicklisp/software/closure-html-...
    patch -p1 < /path/to/gnus-outlook-style/closure-html.diff

Now go back and run `make_binary.sh` again:

    cd /path/to/gnus-outlook-style/format-html-email/
    ./make_binary.sh

This should produce the `format_quoted_email` binary. Copy this to somewhere that is in your `$PATH`.

    cp format_quoted_email ~/bin

Now add this to your emacs init file after `gnus` or `mu4e`, and customize the path to your setup:

    (add-to-list 'load-path "~/.emacs.d/personal/vendor/gnus-outlook-style")
    (setq outlook-style-format-helper-location "~/bin/format_quoted_mail")
    (require 'outlook-style)

If you want `<src>` elements to be nicely formatted, you should also
make sure that the `htmlize` library is installed. This can be
downloaded from ELPA.

Kill and reload emacs:

    emacsclient -e '(kill-emacs)'
    emacs --daemon

**Done, yay!**

Now you are ready to use `gnus-outlook-style`.

Additional Configuration
========================

Dynamic Selection of Posting Style
----------------------------------

If you need to switch between posting with `gnus-outlook-style` and some
other citation style (like one of the citation styles built-in to `gnus`),
you just need to make sure the variable `outlook-style-inhibit` is bound
and not-`nil` when the message buffer is created.  Here's an example of how
to do that using `gnus-posting-styles` and
[OneKey](http://emacswiki.org/emacs/OneKey):

```elisp
(defun my--set-style-traditional ()
  (interactive)
  (set (make-local-variable 'message-cite-reply-position) 'traditional)
  (setq-local 'outlook-style-inhibit t))

(defun my--set-style-traditional ()
  (interactive)
  (set (make-local-variable 'message-cite-reply-position) 'above)
  (setq-local 'outlook-style-inhibit t))

(setq my--reply-style-one-key-menu-alist
      '((("o" . "Outlook") . ignore)
        (("t" . "Traditional") . my--set-style-traditional)
        (("a" . "Above") . my--set-style-above)))

(setq gnus-posting-styles
      '((".*"
         (eval (one-key-menu "reply" my--reply-style-one-key-menu-alist t)))))
```

**TODO:** add instructions on how to actually edit emails in `gnus` and `mu4e`.

A note on the helper application
--------------------------------

The helper application, `format_quoted_email` is written in Common
Lisp. It should build on most CL implementations, although it has
currently only been tested with SBCL.

Note: Currently the application depends on a fix to the CL library
CLOSURE-HTML. Currently, this fix has not been included in this
library's distribution, which is why the following list of steps
include the application of a patch to the library prior to rebuilding
the application.

