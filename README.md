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

Building and installing the helper application
----------------------------------------------

The helper application, `format_quoted_email` is written in Common
Lisp. It should build on most CL implementations, although it has
currently only been tested with SBCL.

Note: Currently the application depends on a fix to the CL library
CLOSURE-HTML. Currently, this fix has not been included in this
library's distribution, which is why the following list of steps
include the application of a patch to the library prior to rebuilding
the application.

To build, perform the following steps:

  1. Make sure SBCL is installed
  2. Make sure Quicklisp is installed (http://www.quicklisp.org/)
  3. Run the script `make_binary.sh`. This will download all the
     required libraries. After this, the compilation will fail since
     the patch has not yet been applied.
  4. Apply the patch `closure-html.diff` to the CLOSURE-HTML
     distribution. Quicklisp generally installs this in
     `$HOME/quicklisp/dists/quicklisp/software/closure-html-*`
  5. Run the script `make_binary.sh` again to make sure the generated
     binary uses the patched library.
  6. You can now copy the generated binary `format_quoted_email` to any
     location (for example, to your `$HOME/.emacs.d` directory.

Installing the elisp code
-------------------------

Add the following to your init script:

```lisp
(add-to-list 'load-path "path-to/gnus-outlook-style")
(require 'outlook-style)
```

You may also wish to configure the customisable variable
`outlook-style-format-helper-location` to the path where you copied
the helper application.

If you want `<src>` elements to be nicely formatted, you should also
make sure that the `htmlize` library is installed. This can be
downloaded from ELPA.
