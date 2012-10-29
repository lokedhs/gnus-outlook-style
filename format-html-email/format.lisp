(in-package :format-html-email)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *html-namespace* "http://www.w3.org/1999/xhtml")
  (defparameter *html-namespaces* (list (list "h" *html-namespace*))))

(defmacro with-html-namespaces (&body body)
  `(xpath:with-namespaces ,*html-namespaces*
     ,@body))

(defun parse-html-content (file)
  (let* ((doc (closure-html:parse file (cxml-dom:make-dom-builder))))
    doc))

(defun external-format-from-name (name)
  (handler-case
      (flexi-streams:make-external-format (intern (string-upcase name) "KEYWORD"))
    (flexi-streams:external-format-error () nil)))

(defun chars-to-string (stream)
  (with-output-to-string (out)
    (loop
       for ch = (read-char stream nil nil)
       while ch
       do (write-char ch out))))

(defun parse-html-content-with-encoding (content format)
  (let* ((in (flexi-streams:make-in-memory-input-stream content))
         (enc (flexi-streams:make-flexi-stream in :external-format format))
         (content-as-string (chars-to-string enc)))
    (parse-html-content content-as-string)))

(defun parse-html-content-with-encoding-safe (content format)
  (handler-case
      (parse-html-content-with-encoding content format)
    (flexi-streams:external-format-encoding-error ()
      (parse-html-content-with-encoding content :iso-8859-1))))

(defun read-stream-to-byte-array (stream)
  (let* ((type '(unsigned-byte 8))
         (buf (make-array (* 1024 16) :element-type type))
         (result (make-array (* 1024 16) :element-type type :adjustable t :fill-pointer 0)))
    (loop
       for length = (read-sequence buf stream)
       do (loop
             repeat length
             for element across buf
             do (vector-push-extend element result))
       while (= length (array-dimension buf 0)))
    result))

(defun plain-parse-bytes-to-string (bytes)
  "Given an array of bytes, return a string consisting of all values in the
array between 0 and 127, converted to a character. In other words, this function
converts an input string into a plain ASCII string, dropping everything that
can't be parsed."
  (let ((result (make-array (array-dimension bytes 0) :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
       for byte across bytes
       when (<= 0 byte 127)
       do (vector-push-extend (code-char byte) result))
    result))

(defun find-charset-from-content-type (content)
  (let ((scanner (cl-ppcre:create-scanner ".*charset=([^ ]*)" :case-insensitive-mode t)))
    (multiple-value-bind (match strings) (cl-ppcre:scan-to-strings scanner content)
      (when match
        (aref strings 0)))))

(defun find-encoding-from-content-type (content)
  (let ((scanner (cl-ppcre:create-scanner "<meta[^>]*http-equiv=(['\"])content-type\\1[^>]*content=(['\"])([^'\"]*)\\2.*>" :case-insensitive-mode t)))
      (multiple-value-bind (match strings) (cl-ppcre:scan-to-strings scanner content)
        (when match
          (find-charset-from-content-type (aref strings 2))))))

(defun parse-html-handle-encoding (stream)
  "Read STREAM and parse it as HTML, returning the corresponding DOM tree.
This function differs from CLOSURE-HTML:PARSE in that it takes the
<meta content> tags into consideration."
  (let* ((content-buffer (read-stream-to-byte-array stream))
         (string (plain-parse-bytes-to-string content-buffer)))
    ;; Now we have a string consisting of just the BASIC LATIN characters from the input
    (let ((encoding (find-encoding-from-content-type string)))
      (if encoding
          ;; We found an encoding, now check if it's valid
          (let ((fmt (external-format-from-name encoding)))
            (if fmt
                ;; We're good, we found a format
                (parse-html-content-with-encoding-safe content-buffer fmt)
                ;; No format. We could parse the ASCII here, but right now it's best to simply bail
                (error "Unknown content type: ~s" encoding)))
          ;; No encoding found in the document, simply parse the buffer using default encoding
          (parse-html-content content-buffer)))))

(defun find-mime-part-by-type (msg type)
  (mime4cl:do-parts (part msg)
    (when (and (typep part 'mime4cl:mime-text)
               (or (null type)
                   (string= (mime4cl:mime-type-string part) type)))
      (return-from find-mime-part-by-type part)))
  nil)

(defun format-plain-text-as-html (part)
  (let ((in (mime4cl:mime-body-stream part :binary nil)))
    (with-output-to-string (out)
      (format out "<html><body><pre>")
      (loop
         for ch = (read-char in nil nil)
         while ch
         do (case ch
              (#\< (write-string "&lt;" out))
              (#\> (write-string "&gt;" out))
              (#\& (write-string "&amp;" out))
              (t   (write-char ch out))))
      (format out "</pre></body></html>"))))

(defun find-content-part (msg)
  "Given a MIME message, return the part that contains the actual email content."
  (alexandria:if-let ((html-part (find-mime-part-by-type msg "text/html")))
    ;; There is a HTML part, return it
    html-part
    ;; No HTML part, check if there is a text part
    (alexandria:if-let ((text (find-mime-part-by-type msg "text/plain")))
      ;; Found text part, create a simple HTML wrapper around the text
      text
      ;; No HTML nor text parts, time to bail
      (error "Unable to find source content"))))

(defun parse-content-as-html (part)
  (string-case:string-case ((mime4cl:mime-type-string part))
    ("text/html" (parse-html-handle-encoding (mime4cl:mime-body-stream part)))
    ("text/plain" (parse-html-handle-encoding (format-plain-text-as-html part)))
    (t "Error: got an unrecognised part type, code possibly out of sync with find-content-part")))

(defvar *quoted-headers* '(("date" "Date:")
                           ("from" "From:")
                           ("to"   "To:")
                           ("cc"   "CC:")))

(defun make-quoted-header (msg doc)
  "Given a MIME message, create a DOM node that contains the quoted
headers from the message."
  (flet ((make-row-node (label message)
           (let ((row (dom:create-element-ns doc *html-namespace* "tr")))
             (let ((td-label (dom:create-element-ns doc *html-namespace* "td")))
               (dom:set-attribute-ns td-label *html-namespace* "style" "font-weight:bold;vertical-align:top;")
               (dom:append-child td-label (dom:create-text-node doc label))
               (dom:append-child row td-label))
             (let ((td-message (dom:create-element-ns doc *html-namespace* "td")))
               (dom:append-child td-message (dom:create-text-node doc message))
               (dom:append-child row td-message))
             row)))

    (let* ((headers (mime4cl:mime-message-headers msg))
           (table (dom:create-element-ns doc *html-namespace* "table"))
           (tbody (dom:create-element-ns doc *html-namespace* "tbody")))
      (dom:append-child table tbody)
      (dolist (h *quoted-headers*)
        (alexandria:when-let ((found-header (find (car h) headers :key #'car :test #'equalp)))
          (dom:append-child tbody (make-row-node (cadr h) (cdr found-header)))))
      table)))

(defun make-mime-attachment-map (part)
  "Create a hash map keyed on the CID of the attachment, with the value
being the mime part. This prevents unneccesary iterating over the message
when there are lots of attachments."
  (let ((h (make-hash-table :test 'equal)))
    (mime4cl:map-parts #'(lambda (v)
                           (multiple-value-bind (match strings)
                               (cl-ppcre:scan-to-strings "^ *<(.*)> *$" (mime4cl:mime-id v))
                             (when match
                               (setf (gethash (aref strings 0) h) v))))
                       part)
    h))

(defun copy-attachment-to-file (part tmp-dir)
  "Write the content of attachment PART into a newly created
file in TMP-DIR and return the pathname of the file."
  (let ((*default-pathname-defaults* tmp-dir))
    (temporary-file:with-open-temporary-file (s :keep t :template "html-format-attachment-tmp%" :element-type '(unsigned-byte 8))
      (mime4cl:with-input-from-mime-body-stream (in part)
        (cl-fad:copy-stream in s))
      (pathname s))))

(defun extract-inline-images (doc part tmp-dir)
  "Find all 'img' tags in the HTML document which has a 'src' attribute
that points to an internal image \(i.e. their URL begins with 'cid:'), and
extract the corresponding images into individual files."
  (let ((attachments (make-mime-attachment-map part))
        (images-map nil))
    (xpath:map-node-set #'(lambda (node)
                            (multiple-value-bind (match strings)
                                (cl-ppcre:scan-to-strings "^cid:(.*)$" (dom:get-attribute node "src"))
                              (when match
                                (let* ((id (aref strings 0))
                                       (image (gethash id attachments)))
                                  (when image
                                    (push (list id
                                                (mime4cl:mime-type-string image)
                                                (copy-attachment-to-file image tmp-dir))
                                          images-map))))))
                        (xpath:evaluate "//h:img" doc))
    images-map))

(defparameter *source-style* (concatenate 'string
                                          "border: 1pt solid #000000;"
                                          "background-color: #fbfbfb;"
                                          "padding: 5pt;"
                                          "font-family: monospace;"
                                          "font-size: 90%;"
                                          "overflow: auto;"
                                          "margin-bottom: 1em;"))

(defun update-styles-for-tree (div)
  "Update the style attribute for relevant nodes. Note that the fact that the nodes
themselves are updated instead of adding a <style> section is intentional. There is
no way to make sure that other email clients doesn't mess with the styles, causing
quoted emails to look bad."
  (xpath:map-node-set #'(lambda (node)
                          (dom:remove-attribute node "class")
                          (dom:set-attribute-ns node *html-namespace* "style" *source-style*))
                      (xpath:evaluate "//h:pre[@class='src']" div)))

(defun quote-email (content old-content tmp-dir &optional (stream *standard-output*))
  (with-html-namespaces 
    (let* ((msg (mime4cl:mime-message (pathname old-content)))
           (content-doc (parse-html-content content))
           (old-content-part (find-content-part msg))
           (old-content-doc (parse-content-as-html old-content-part)))

      (let ((div (dom:create-element-ns old-content-doc *html-namespace* "div")))
        (dom:set-attribute-ns div *html-namespace* "style" "font-family: Helvetica, sans-serif;")

        ;; Copy the new content into a div, which will subsequently be inserted into the old document
        (loop
           for node across (dom:child-nodes (xpath:first-node (xpath:evaluate "/h:html/h:body" content-doc)))
           do (dom:append-child div (dom:import-node old-content-doc node t)))

        ;; Process the div to insert explicit style attributes
        ;;(update-styles-for-tree div)

        ;; Insert the div that holds the new content, as well as the divider at the beginning of the old document
        (let ((n (xpath:first-node (xpath:evaluate "/h:html/h:body" old-content-doc)))
              (divider (dom:create-element-ns old-content-doc *html-namespace* "hr"))
              (quoted-header (make-quoted-header msg old-content-doc))
              (separator (dom:create-element-ns old-content-doc *html-namespace* "p")))
          (dom:insert-before n separator (dom:first-child n))
          (dom:insert-before n quoted-header separator)
          (dom:insert-before n divider quoted-header)
          (dom:insert-before n div divider)))

      (dolist (v (extract-inline-images old-content-doc msg tmp-dir))
        (format stream "~a~%~a~%~a~%" (car v) (cadr v) (caddr v)))
      (format stream "==END==~%")

      (dom:map-document (cxml:make-namespace-normalizer (closure-html:make-character-stream-sink stream)) old-content-doc))))

(defun main ()
  (handler-case
      (let ((args (command-line-arguments:get-command-line-arguments)))
        (unless (= (length args) 3)
          (error "Usage: content old-content image-directory"))
        (quote-email (pathname (nth 0 args))
                     (pathname (nth 1 args))
                     ;; Append an extra / to make sure the directory name is not
                     ;; interpreted as a file name
                     (merge-pathnames (concatenate 'string (nth 2 args) "/"))))

    (error (cond)
      (trivial-backtrace:print-condition cond *error-output*)
      (trivial-shell:exit :failure))))
