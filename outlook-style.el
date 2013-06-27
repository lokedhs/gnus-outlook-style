;;; -*- lexical-binding: t -*-

;;; cl is needed everywhere
(require 'cl)

;;;
;;; Muse for gnus
;;;
(require 'muse-publish)
(require 'muse-html)

(defgroup outlook-style nil "Customisation for outlook-style" :prefix 'outlook-style)

;;;###autoload
(defcustom outlook-style-format-helper-location "format_quoted_mail"
  "The program used to merge the HTML content."
  :type 'string
  :group 'outlook-style)

;;;###autoload
(defcustom outlook-style-body-css "font-family: Helvetica, sans-serif; font-size: 12;"
  "The style applied to the body element of the generated HTML."
  :type 'string
  :group 'outlook-style)

;;;###autoload
(defcustom outlook-style-src-css "border: 1pt solid #b0b0b0; background-color: #e8e8e8; padding: 5pt; font-family: monospace; font-size: 90%; overflow: auto;"
  "The style that is applied to src elements."
  :type 'string
  :group 'outlook-style)

;;;###autoload
(defcustom outlook-style-code-css "background: #e8e8e8; padding-left: 2px; padding-right: 2px;"
  "The style that is applied to code elements."
  :type 'string
  :group 'outlook-style)

;;;###autoload
(defcustom outlook-style-header-title-css "font-family: Helvetica, sans-serif; font-size: 12; font-weight:bold; vertical-align:top;"
  "Style for the left column in the quoted email header."
  :type 'string
  :group 'outlook-style)

;;;###autoload
(defcustom outlook-style-header-data-css "font-family: Helvetica, sans-serif; font-size: 12;"
  "Style for the right column in the quoted email header."
  :type 'string
  :group 'outlook-style)

;;;###autoload
(defcustom outlook-style-init-hook nil
  "Hook that is called after the buffer has been set up for outlook-style editing."
  :type 'hook
  :group 'outlook-style)

;;;###autoload
(defcustom outlook-style-temporary-dir "/tmp"
  "The directory where temporary files are saved to."
  :type 'directory
  :group 'outlook-style)

(defvar outlook-style-post-active nil
  "This variable is set to true when the outlook-style psting should
be activated. This is used by the advice on the function `gnus-post-news'
so that messages sent using other methods are not affected.")

(defvar outlook-style-conf-start "============ Outlook style settings ============")
(defvar outlook-style-conf-end "============ End of settings ============")
(defvar outlook-style-option-prefix "##")

(defun llog (fmt &rest args)
  (with-current-buffer (get-buffer-create "llog")
    (insert (apply #'format fmt args) "\n")))

(defmacro with-temp-files (files &rest body)
  (declare (indent 1))
  (if files
      (let ((f (car files))
            (temp-file-name (gensym)))
        `(let ((,temp-file-name (make-temp-file ,(cadr f))))
           (let ((,(car f) ,temp-file-name))
             (unwind-protect
                 (with-temp-files ,(cdr files)
                   ,@body)
               (delete-file ,temp-file-name)))))
    `(progn ,@body)))

(defun outlook-style--remove-inline-mail-content (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "<#" nil t)
      (let ((begin (- (point) 2)))
        (let ((quoted nil)
              (escaped nil))
          (while (or quoted (not (eql (char-after (point)) ?>)))
            (let ((ch (char-after (point)))
                  (next-character-escaped nil))
              (case ch
                (?\\ (when (and quoted (not escaped)) (setq next-character-escaped t)))
                (?\" (unless escaped (setq quoted (not quoted)))))
              (setq escaped next-character-escaped)
              (forward-char))))
        (forward-char)
        (delete-region begin (point))))
    (buffer-string)))

(defun outlook-style--collect-and-remove (pattern)
  "Return a list of all strings matching `pattern' in the current buffer,
and remove the occurrences."
  (save-excursion
    (goto-char (point-min))
    (loop while (search-forward-regexp pattern nil t)
          collect (match-string 0)
          do (replace-match ""))))

(defun outlook-style--remove-and-get-inline-mail-content (text)
  "Remove inline attachment specifications. This function returns
a list where the first element consists of the resulting email
without attachments, the second element being a list of the
extracted attachment specifications and the third element is a
list of tags that should go outside the multipart section."
  (with-temp-buffer
    (insert text)
    (let ((attachments (outlook-style--collect-and-remove "<#part [^>]*disposition=attachment[^>]*>\\(.\\|\n\\)*<#/part>"))
          (rem (outlook-style--collect-and-remove "<#secure[^>]*>")))
      (list (buffer-string) attachments rem))))

(defun outlook-style--publish-and-update-tags ()
  "Convert the markup in the current buffer to HTML"
  (let ((content (buffer-string))
        new-content)
    ;; Publish the HTML to a separate buffer since muse-publish-markup-buffer will
    ;; add read-only properites to the output that has to be removed before we
    ;; can change the content.
    (with-temp-buffer
      (insert content)
      (muse-publish-markup-buffer nil "html")
      (setq new-content (substring-no-properties (buffer-string))))
    (delete-region (point-min) (point-max))
    (insert new-content)))

(defun outlook-style--process-source-email (content file)
  "Convert the email content to HTML using Muse and write the output to FILE"
    (with-temp-buffer
      (insert content)
      (outlook-style--publish-and-update-tags)
      (write-file file)))

(defun outlook-style--get-parent-reference ()
  "Return a reference to the parent mail. The CAR of the returned list
is a symbol describing the source of the mail (mu, gnus-m...) and the
CADR is the source-specific data."
  (cond ((and (fboundp 'mu4e) (boundp 'mu4e-compose-parent-message) mu4e-compose-parent-message)
         (list 'mu mu4e-compose-parent-message))

        ((not (null outlook-style-gnus-article-current-copy))
         (let ((id (cdr outlook-style-gnus-article-current-copy))
               (message (car outlook-style-gnus-article-current-copy)))
           (list 'gnus-m (list id message))))

        (t
         (error "Unable to find parent reference"))))

(defun outlook-style--get-email (reference file)
  "Read the email referenced by REFERENCE into the file with name FILE."
  (ecase (car reference)
    (mu (destructuring-bind (&key path &allow-other-keys) (cadr reference)
          (copy-file path file t)))
    (gnus-m (let ((l (cadr reference)))
              (with-temp-buffer
                (gnus-request-article (car l) (cadr l) (current-buffer))
                (write-file file))))))

(defun outlook-style--escape-lisp (s)
  (with-output-to-string
    (loop for ch across s
          do (princ (case ch
                      (?\\ "\\\\")
                      (?\" "\\\"")
                      (10  "\\n")
                      (13  "\\r")
                      (t   (char-to-string ch)))))))

(defun outlook-style--write-styles-settings (file)
  (flet ((print-param (name value) (insert (concat name "=" (outlook-style--escape-lisp value) "\n"))))
    (with-temp-buffer
      (print-param "body" outlook-style-body-css)
      (print-param "src" outlook-style-src-css)
      (print-param "code" outlook-style-code-css)
      (print-param "header-title" outlook-style-header-title-css)
      (print-param "header-data" outlook-style-header-data-css)
      (write-file file))))

(defun outlook-style--get-next-pair ()
  (flet ((line-content ()
                       (when (looking-at "==END==")
                         (error "Illegal attachment format"))
                       (let ((s (outlook-style--remove-trailing-newlines (thing-at-point 'line))))
                         (forward-line)
                         s)))

    (let* ((id (line-content))
           (type (line-content))
           (file (line-content)))
      (list (format "<#part type=%s name=\"%s\" id=\"<%s>\" filename=\"%s\" description=\"%s\" disposition=inline>\n<#/part>"
                    type id id file id)
            file))))

(defun outlook-style--call-email-format (new-message old-message attachment-list remaining-sections)
  (with-temp-files ((styles-filename "styles"))
    (outlook-style--write-styles-settings styles-filename)

    (with-temp-buffer
      (let ((error-buffer (get-buffer-create "*format-quoted-email errors*")))
        (unless (zerop (shell-command (format "%s '%s' '%s' '%s' '%s'"
                                              (expand-file-name outlook-style-format-helper-location)
                                              new-message old-message styles-filename
                                              (expand-file-name outlook-style-temporary-dir))
                                      (current-buffer) error-buffer))
          (switch-to-buffer error-buffer)
          (error "Formatting of email failed")))

      (goto-char (point-min))
      (let ((images (loop while (not (looking-at "==END=="))
                          collect (outlook-style--get-next-pair))))
        (forward-line) ; Skip past the ==END== marker so it gets deleted too
        (when (= (point) (point-max))
          (error "Reached the end of the buffer while processing attachments"))
        (delete-region (point-min) (point))

        (list (buffer-string)
              (append attachment-list (mapcar #'car images))
              remaining-sections
              (mapcar #'cadr images))))))

(defun outlook-style--generate-quoted-html (new-content)
  "Given the new email's content, combine it with the old email thread and
generate the resulting HTML. This function returns a list of three
elements: the new email content as a string, a list of attachments to
be added to the end of the mail and a list of files to be deleted after
the email has been created."
  (let ((processed-results (outlook-style--remove-and-get-inline-mail-content new-content)))
    (let ((ref outlook-style-local-yank))
      (with-temp-files ((new-message "email-new")
                        (old-message "email-old"))
        (outlook-style--process-source-email (car processed-results) new-message)
        (outlook-style--get-email ref old-message)
        (outlook-style--call-email-format new-message old-message
                                          (cadr processed-results) (caddr processed-results))))))

(defun outlook-style--simple-muse-message (content)
  (let ((processed-results (outlook-style--remove-and-get-inline-mail-content content)))
    (with-temp-files ((file "email-new"))
      (outlook-style--process-source-email (car processed-results) file)
      (outlook-style--call-email-format file "new"
                                        (cadr processed-results) (caddr processed-results)))))

(defun outlook-style--remove-trailing-newlines (s)
  (loop for i from (1- (length s)) downto 0
        while (member (aref s i) '(?\n ?\r))
        finally (return (subseq s 0 (1+ i)))))

(defun outlook-style--find-end-of-settings ()
  "Return the position of the end of the settings-end marker or
the value of (point-max) if the marker can't be found."
  (save-excursion
    (if (search-forward-regexp (concat "^" (regexp-quote outlook-style-conf-end) "$") nil t)
        (point)
      (point-max))))

(defun outlook-style--mail-body-start-point ()
  "Return the location where point would go if (message-goto-body) is called."
  (save-excursion
    (message-goto-body)
    (point)))

(defun outlook-style--prepare-buffer-for-submit (new-content old-content processed-results)
  (destructuring-bind (content attachments remaining-sections files-to-delete) processed-results
    (message-goto-body)
    (delete-region (point) (point-max))
    (insert "<#multipart type=alternative>\n")
    (insert (outlook-style--remove-inline-mail-content new-content))
    (when (plusp (length old-content))
      (insert "\n===============================\n")
      (insert (outlook-style--remove-inline-mail-content old-content) "\n"))
    (when attachments
      (insert "<#multipart type=related>\n"))
    (insert "<#part type=text/html>\n")
    (insert content "\n")
    (when attachments
      (dolist (attachment attachments)
        (insert attachment "\n"))
      (insert "<#/multipart>\n"))
    (insert "<#/multipart>\n")
    ;; Insert the remaining sections at the end of the email
    (dolist (sec remaining-sections)
      (insert sec "\n"))

    ;; If there are files to be deleted, add them to the buffer-local list
    (when files-to-delete
      (set (make-local-variable 'outlook-style-local-temporary-files) files-to-delete))))

(defun outlook-style--find-all-options-from-config (end-of-settings)
  (loop while (search-forward-regexp (concat "^" (regexp-quote outlook-style-option-prefix)
                                             " *\\([a-zA-Z_-]+\\) +.*$")
                                     end-of-settings t)
        collect (match-string 1)))

(defun outlook-style--call-muse-for-message ()
  (save-excursion
    (message-goto-body)
    (when (search-forward-regexp (concat "^" (regexp-quote outlook-style-conf-start) "$") nil t)
      (forward-line 0)
      (let ((start-of-settings (point))
            (end-of-settings (outlook-style--find-end-of-settings)))
        ;; Skip the config settings marker
        (forward-line 1)
        ;; Extract the options from the configuration section
        (let* ((options (outlook-style--find-all-options-from-config end-of-settings))
               (format-muse (find "format_muse" options :test #'equal))
               (include-old (find "quote_history" options :test #'equal))
               (new-content (buffer-substring (outlook-style--mail-body-start-point) start-of-settings))
               (old-content (buffer-substring end-of-settings (point-max))))

          ;; Check for invalid settings
          (when (and (not format-muse) include-old)
            (error "The old email chain can only be included when 'format_muse' is enabled."))

          (if format-muse
              (let ((processed-results (if include-old
                                           ;; The user wants to include the old email chain, "outlook style"
                                           (outlook-style--generate-quoted-html new-content)
                                         ;; The old email chain should not be included
                                         (outlook-style--simple-muse-message new-content))))
                (outlook-style--prepare-buffer-for-submit new-content old-content processed-results))
                
            ;; The user does not want to use muse, so simply
            ;; delete the configuration section from the buffer
            ;; and fall back to the default mailing style
            (delete-region start-of-settings end-of-settings)))))))

(defun outlook-style--gnus-prepare ()
  (when (and outlook-style-post-active
             (not (save-excursion (message-goto-body) (search-forward "<#mml" nil t))))
    (let ((replyp (save-excursion
                    (message-goto-body)
                    (cond ((= (point) (point-max))
                           nil)
                          (t
                           (replace-regexp "^> ?\\(.*\\)$" "\\1" nil (point) (point-max))
                           t)))))

      (message-goto-body)
      (insert "\n\n" outlook-style-conf-start "\n")

      (insert "This mail buffer is configured to use the outlook-style package. Lines\n")
      (insert "below beginning with " outlook-style-option-prefix " indicates enabled options.\n")

      (insert "The name of the option follows immediately after the option prefix.\n")
      (insert "Any words after the option names are ignored. Any line in this block\n")
      (insert "that does not start with " outlook-style-option-prefix " is also ignored.\n\n")

      (insert outlook-style-option-prefix " format_muse   Format the email content using Muse markup\n")
      (when replyp
        (insert outlook-style-option-prefix " quote_history Include the previous email chain below the content\n"))
      (insert outlook-style-conf-end "\n\n")

      (if replyp
          (progn
            (set (make-local-variable 'outlook-style-local-yank) (outlook-style--get-parent-reference))
            (message-goto-body))
        (message-goto-to))))
  (run-hooks 'outlook-style-init-hook))

(defun outlook-style--cleanup-temporary-attachments ()
  (when (boundp 'outlook-style-local-temporary-files)
    (dolist (f outlook-style-local-temporary-files)
      (delete-file f))))

(add-hook 'gnus-message-setup-hook 'outlook-style--gnus-prepare)
(add-hook 'message-send-hook 'outlook-style--call-muse-for-message)
(add-hook 'message-sent-hook 'outlook-style--cleanup-temporary-attachments)

(defvar outlook-style-gnus-article-current-copy nil
  "Dynamic variable that contains a reference to the original email.")

(defadvice gnus-post-news (around outlook-style-post
                                  (post &optional
                                        group header article-buffer yank subject force-news))
  (when (> (length yank) 1)
    (error "More than one message selected for reply"))
  (let ((message (car yank)))
    ;; This stuff is a bit of a mess. In this function, the variable `yank' is
    ;; a list of elements that can have any of the following values:
    ;;
    ;;   nil:  No quoting should be done
    ;;   atom: The value is a message index (id of a message in a given group)
    ;;   list: Multiple messages as above
    ;;   list of list: Multiple messages, where the car is the index and the cdr is
    ;;                 the part of the message that should be quoted
    (let ((outlook-style-gnus-article-current-copy
           (cond ((null message)
                  nil)
                 ((listp message)
                  (if (null (cdr message))
                      (cons group (car message))
                    nil))
                 ((numberp message)
                  (cons group message))
                 (t
                  "Error outlook-style was not prepared to handle a yank of value: %s" yank))))
      ad-do-it)))

(ad-activate 'gnus-post-news)

(defmacro outlook-style--advice-followup-function (function)
  (let ((n (intern (concat "outlook-style-" (symbol-name function) "-around"))))
    `(progn
       (defadvice ,function (around ,n)
         (let ((outlook-style-post-active t))
           ad-do-it))
       (ad-activate ',function))))

(outlook-style--advice-followup-function gnus-summary-followup-with-original)
(outlook-style--advice-followup-function gnus-article-followup-with-original)
(outlook-style--advice-followup-function gnus-group-mail)
(outlook-style--advice-followup-function gnus-summary-mail-other-window)

;;;
;;;  Setup for mu4e
;;;

(defun outlook-style--mu4e-compose (compose-type &optional original-msg includes)
  (mu4e~compose-handler compose-type original-msg includes)
  (outlook-style--gnus-prepare))

(defun outlook-style-setup-mu4e ()
  (setq mu4e-compose-func 'outlook-style--mu4e-compose)
  (defadvice mu4e~compose-handler (after mu4e-compose-adafter (compose-type &optional original-msg includes))
    (when (eq compose-type 'new)
      (outlook-style--gnus-prepare)))
  (ad-activate 'mu4e~compose-handler))

(when (fboundp 'mu4e)
  (outlook-style-setup-mu4e))

(require 'outlook-style-muse-editor)

(provide 'outlook-style)
