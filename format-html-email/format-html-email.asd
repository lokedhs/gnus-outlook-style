(asdf:defsystem format-html-email
  :name "format-html-email"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :description "Updates the quote part of an HTML email"
  :depends-on (:closure-html
               :cxml
               :xpath
               :command-line-arguments
               :getopt
               :trivial-backtrace
               :mime4cl
               :alexandria
               :string-case
               :cl-ppcre
               :cl-fad
               :temporary-file
               :trivial-shell
               :uiop
               :iconv)
  :serial t
  :components ((:file "package")
               (:file "format")))
