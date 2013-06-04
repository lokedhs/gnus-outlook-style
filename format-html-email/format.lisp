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
      (let ((name-as-symbol (intern (string-upcase name) "KEYWORD")))
        (babel:make-external-format (if (eq name-as-symbol :gb2312)
                                        :gbk
                                        name-as-symbol)))
    (simple-error () nil)))

(defun parse-styles-file (file)
  (labels ((remove-escapes (s)
             (with-output-to-string (out)
               (loop
                  with next-escape = nil
                  for ch across s
                  do (cond ((not (null next-escape))
                            (write-char (ecase ch
                                          (#\n #\Newline)
                                          (#\r #\Return)
                                          (#\\ #\\)
                                          (#\" #\"))
                                        out)
                            (setq next-escape nil))
                           ((char= ch #\\)
                            (setq next-escape t))
                           (t
                            (write-char ch out)))
                  finally (when next-escape
                            (error "Escaped string ends with backslash"))))))

    (with-open-file (in file)
      (loop
         for s = (read-line in nil nil)
         while s
         collect (multiple-value-bind (match strings)
                     (cl-ppcre:scan-to-strings "^([a-zA-Z-]+)=(.*)$" s)
                   (unless match
                     (error "Cannot parse styles line: ~s" s))
                   (cons (aref strings 0) (remove-escapes (aref strings 1))))))))

(defun find-css (styles name)
  (let ((v (assoc name styles :test #'equal)))
    (unless v
      (error "Can't find style \"~a\"" name))
    (cdr v)))

(defun parse-html-content-with-encoding (content format)
  (flet ((parse (format)
           (let ((content-as-string (babel:octets-to-string content :encoding format)))
             (parse-html-content content-as-string))))
    (handler-case
        (parse format)
      (babel-encodings:character-decoding-error ()
        (parse :iso-8859-1)))))

(defun read-stream-to-byte-array (stream)
  "Read a sequence of \(UNSIGNED-BYTE 8) into an array with this element type."
  (let* ((type '(unsigned-byte 8))
         ;; Note that the buffer does not use (STREAM-ELEMENT-TYPE STREAM) as the
         ;; element type. This is because mime4cl is broken and will sometimes
         ;; return values outside of the valid range of the element type.
         ;;
         ;; In the specific case that caused this fix to be implemented, mime4cl
         ;; returned a stream with element type (UNSIGNED-BYTE 8) but returned
         ;; a value of 8211 (which happens to be the Unicode character EM DASH)
         (buf (make-array (* 1024 16) :element-type 'unsigned-byte))
         (result (make-array (* 1024 16) :element-type type :adjustable t :fill-pointer 0)))
    (handler-bind (#+sbcl(sb-int:stream-decoding-error
                          #'(lambda (condition)
                              (declare (ignore condition))
                              (invoke-restart 'sb-int:attempt-resync))))
      (loop
         for length = (read-sequence buf stream)
         do (loop
               repeat length
               for element across buf
               when (typep element type)
               do (vector-push-extend element result))
         while (= length (array-dimension buf 0))))
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

(defun iconv-to-string (from-code from-vector)
  (defparameter *src* from-vector)
  (let ((result (iconv:iconv from-code :utf-32be from-vector)))
    (unless (zerop (mod (length result) 4))
      (error "UTF-32BE output length is not divisible by 4: ~a" (length result)))
    (with-output-to-string (out)
      (dotimes (i (/ (length result) 4))
        (write-char (code-char (logior (ash (aref result (* i 4)) 24)
                                       (ash (aref result (+ (* i 4) 1)) 16)
                                       (ash (aref result (+ (* i 4) 2)) 8)
                                       (aref result (+ (* i 4) 3))))
                    out)))))

(defun parse-html-handle-encoding (stream preferred-encoding)
  "Read STREAM and parse it as HTML, returning the corresponding DOM tree.
This function differs from CLOSURE-HTML:PARSE in that it takes the
<meta content> tags into consideration. PREFERRED-ENCODING is the
encoding to try to use first, before looking at any other encodings."
  (let ((content-buffer (read-stream-to-byte-array stream)))
    (if preferred-encoding
        ;; If we have a preferred encoding, just use it
        (parse-html-content (iconv-to-string preferred-encoding content-buffer))
        ;; Otherwise, look at the meta tags in the html
        (let ((string (plain-parse-bytes-to-string content-buffer)))
          ;; Now we have a string consisting of just the BASIC LATIN characters from the input
          (let ((encoding (find-encoding-from-content-type string)))
            (if encoding
                ;; We found an encoding, now check if it's valid
                (let ((fmt (external-format-from-name encoding)))
                  (if fmt
                      ;; We're good, we found a format
                      (parse-html-content-with-encoding content-buffer fmt)
                      ;; No format. We could parse the ASCII here, but right now it's best to simply bail
                      (error "Unknown content type: ~s" encoding)))
                ;; No encoding found in the document, simply parse the buffer using default encoding
                (parse-html-content-with-encoding content-buffer :utf-8)))))))

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

(defun string-as-utf-8-stream (string)
  (flexi-streams:make-in-memory-input-stream (babel:string-to-octets string :encoding :utf-8)))

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

(defun charset-from-mime (part)
  "Given PART, return the charset that is specified in its charset section.
Return NIL if there is no charset, or the charset isn't parseable."
  (let ((charset (assoc "charset" (mime4cl:mime-type-parameters part) :test #'equal)))
    (when (stringp (cdr charset))
      (cdr charset))))

(defun parse-content-as-html (part)
  (let ((encoding (charset-from-mime part)))
    (string-case:string-case ((mime4cl:mime-type-string part))
      ("text/html" (parse-html-handle-encoding (mime4cl:mime-body-stream part) encoding))
      ("text/plain" (parse-html-handle-encoding (string-as-utf-8-stream (format-plain-text-as-html part)) encoding))
      (t "Error: got an unrecognised part type, code possibly out of sync with find-content-part"))))

(defvar *quoted-headers* '(("date" "Date:")
                           ("from" "From:")
                           ("to"   "To:")
                           ("cc"   "CC:")))

(defun make-quoted-header (msg doc styles)
  "Given a MIME message, create a DOM node that contains the quoted
headers from the message."
  (let ((header-title-style (find-css styles "header-title"))
        (header-data-style (find-css styles "header-data")))

    (flet ((make-row-node (label message)
             (let ((row (dom:create-element-ns doc *html-namespace* "tr")))
               (let ((td-label (dom:create-element-ns doc *html-namespace* "td")))
                 (dom:set-attribute-ns td-label *html-namespace* "style" header-title-style)
                 (dom:append-child td-label (dom:create-text-node doc label))
                 (dom:append-child row td-label))
               (let ((td-message (dom:create-element-ns doc *html-namespace* "td")))
                 (dom:set-attribute-ns td-message *html-namespace* "style" header-data-style)
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
        table))))

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

(defun find-extension (name)
  (let ((index (position #\. name :from-end t)))
    (if index
        (subseq name (1+ index))
        nil)))

(defun copy-attachment-to-file (part tmp-dir)
  "Write the content of attachment PART into a newly created
file in TMP-DIR and return the pathname of the file."
  (let* ((*default-pathname-defaults* tmp-dir)
         (ext (find-extension (mime4cl:mime-part-file-name part)))
         (template (format nil "html-format-attachment-tmp%~a" (if ext (concatenate 'string "." ext) ""))))
    (temporary-file:with-open-temporary-file (s :keep t :template template :element-type '(unsigned-byte 8))
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

(defun generate-random-class-id ()
  "Returns a random style class id"
  (with-output-to-string (s)
    (format s "email-")
    (loop
       repeat 20
       do (write-char (code-char (+ (random (1+ (- (char-code #\z)
                                                   (char-code #\a))))
                                    (char-code #\a))) s))))

(defun insert-node-first (node child)
  "Inserts CHILD as the first chile element of NODE."
  (if (dom:has-child-nodes node)
      (dom:insert-before node child (dom:first-child node))
      (dom:append-child node child)))

(defun remove-initial-newline-from-pre (doc node)
  "If the text content of NODE begins with a newline, then remove it."
  (loop
     for n across (copy-seq (dom:child-nodes node))
     when (dom:text-node-p n)
     do (progn
          (let ((text (dom:node-value n)))
            (when (and (plusp (length text)) (char= (aref text 0) #\Newline))
              (when (> (length text) 1)
                (dom:insert-before node (dom:create-text-node doc (subseq text 1)) n))))
              (dom:remove-child node n)
          (return))))

(defun update-styles-for-tree (doc div styles)
  "Update the style attribute for relevant nodes. Note that the fact that the nodes
themselves are updated instead of adding a <style> section is intentional. There is
no way to make sure that other email clients doesn't mess with the styles, causing
quoted emails to look bad."
  (let ((body-style (find-css styles "body"))
        (class-id (generate-random-class-id))
        (head (xpath:first-node (xpath:evaluate "/h:html/h:head" doc)))
        (style-element (dom:create-element-ns doc *html-namespace* "style")))
    (dom:set-attribute-ns div *html-namespace* "class" class-id)
    (dom:append-child style-element (dom:create-text-node doc (with-output-to-string (out)
                                                                (format out "div.~a { ~a }~%" class-id body-style)
                                                                (format out "div.~a p { ~a }" class-id body-style))))
    (dom:append-child head style-element))

  (let ((src-style (find-css styles "src")))
    (xpath:map-node-set #'(lambda (node)
                            (dom:remove-attribute node "class")
                            (dom:set-attribute-ns node *html-namespace* "style" src-style)
                            (let ((parent (dom:parent-node node))
                                  (div (dom:create-element-ns doc *html-namespace* "div")))
                              (dom:insert-before parent div node)
                              (dom:remove-child parent node)
                              (dom:set-attribute-ns div *html-namespace* "style" "margin-bottom: 1em;")
                              (dom:append-child div node))
                            ;; Because Outlook does not comply with the HTML spec (it uses the IE6 renderer?),
                            ;; we need to remove the first newline in the <pre> section. Otherwise it
                            ;; will render an initial newline in the block.
                            (remove-initial-newline-from-pre doc node))
                        (xpath:evaluate "//h:pre[@class='src']" div)))

  (let ((code-style (find-css styles "code")))
    (xpath:map-node-set #'(lambda (node)
                            (dom:set-attribute-ns node *html-namespace* "style" code-style))
                        (xpath:evaluate "//h:code" div))))

(defun quote-email (content old-content tmp-dir styles &optional (stream *standard-output*))
  (with-html-namespaces 
    (let* ((msg (mime4cl:mime-message (pathname old-content)))
           (content-doc (parse-html-content content))
           (old-content-part (find-content-part msg))
           (old-content-doc (parse-content-as-html old-content-part)))

      (let ((div (dom:create-element-ns old-content-doc *html-namespace* "div")))
        ;; Copy the new content into a div, which will subsequently be inserted into the old document
        (loop
           for node across (copy-seq (dom:child-nodes (xpath:first-node (xpath:evaluate "/h:html/h:body" content-doc))))
           do (dom:append-child div (dom:import-node old-content-doc node t)))

        ;; Process the div to insert explicit style attributes
        (update-styles-for-tree old-content-doc div styles)

        ;; Insert the div that holds the new content, as well as the divider at the beginning of the old document
        (let ((n (xpath:first-node (xpath:evaluate "/h:html/h:body" old-content-doc)))
              (divider (dom:create-element-ns old-content-doc *html-namespace* "hr"))
              (quoted-header (make-quoted-header msg old-content-doc styles))
              (separator (dom:create-element-ns old-content-doc *html-namespace* "p")))
          (dom:insert-before n separator (dom:first-child n))
          (dom:insert-before n quoted-header separator)
          (dom:insert-before n divider quoted-header)
          (dom:insert-before n div divider)))

      (dolist (v (extract-inline-images old-content-doc msg tmp-dir))
        (format stream "~a~%~a~%~a~%" (car v) (cadr v) (caddr v)))
      (format stream "==END==~%")

      (dom:map-document (cxml:make-namespace-normalizer (closure-html:make-character-stream-sink stream)) old-content-doc))))

(defun rewrite-new-email (content styles &optional (stream *standard-output*))
  (with-html-namespaces
    (let* ((content-doc (parse-html-content content))
           (div (dom:create-element-ns content-doc *html-namespace* "div")))
      ;; Move all the body content into the div
      (let ((body-node (xpath:first-node (xpath:evaluate "/h:html/h:body" content-doc))))
        (loop
           for node across (copy-seq (dom:child-nodes body-node))
           do (progn
                (dom:remove-child body-node node)
                (dom:append-child div node)))
        ;; Insert the div into the (now empty) body node
        (dom:append-child body-node div)
        ;; Update the styles according to preferences
        (update-styles-for-tree content-doc div styles)
        ;; Since we know there are no inline attachments, just write the end tag
        (format stream "==END==~%")
        ;; Output the rest of the document to the output stream
        (dom:map-document (cxml:make-namespace-normalizer (closure-html:make-character-stream-sink stream)) content-doc)))))

(defun main ()
  (handler-case
      (progn
        (setq *random-state* (make-random-state t))
        (let ((args #+sbcl (cdr sb-ext:*posix-argv*)
                    #-sbcl (command-line-arguments:get-command-line-arguments)))
          (unless (= (length args) 4)
            (error (format nil "Usage: content old-content image-directory styles-file")))
          (let ((new-content (pathname (nth 0 args)))
                (old-content (let ((f (nth 1 args))) (if (string= f "new") nil (pathname f))))
                ;; Append an extra / to make sure the directory name is not
                ;; interpreted as a file name
                (tmp-dir (merge-pathnames (concatenate 'string (nth 2 args) "/")))
                (styles (parse-styles-file (nth 3 args))))
            (if old-content
                (quote-email new-content old-content tmp-dir styles)
                (rewrite-new-email new-content styles)))))

    (error (cond)
      (trivial-backtrace:print-condition cond *error-output*)
      (trivial-shell:exit :failure))))
