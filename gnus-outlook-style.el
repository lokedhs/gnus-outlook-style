;;; -*- lexical-binding: t -*-

;;; cl is needed everywhere
(require 'cl)

;;;
;;; Muse for gnus
;;;
(require 'muse-publish)
(require 'muse-html)

(defcustom outlook-format-program "format_quoted_mail"
  "The program used to merge the HTML content.")

(defvar mail-message-divider "======== END OF MESSAGE ========")
(defvar email-muse-format-marker "====== Muse format marker ======")

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

(defun remove-inline-mail-content (text)
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

(defun remove-and-get-inline-mail-content (text)
  "Remove inline attachment specifications. This function returns
a list where the first element consists of the resulting email
without attachments, and the second element being a list of the
extracted attachment specifications."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (loop while (search-forward-regexp "<#part [^>]*disposition=attachment[^>]*>\\(.\\|\n\\)*<#/part>" nil t)
          collect (match-string 0) into attachments
          do (replace-match "")
          finally (return (list (buffer-string) attachments)))))

(defun get-next-pair ()
  (cl-flet ((line-content ()
                          (when (looking-at "==END==")
                            (error "Illegal attachment format"))
                          (let ((s (remove-trailing-newline (thing-at-point 'line))))
                            (forward-line)
                            s)))

    (let* ((id (line-content))
           (type (line-content))
           (file (line-content)))
      (list (format "<#part type=%s name=\"%s\" id=\"<%s>\" filename=\"%s\" description=\"%s\">\n<#/part>"
                    type id id file id)
            file))))

(defun generate-quoted-html (new-content)
  "Given the new email's content, combine it with the old email thread and
generate the resulting HTML. This function returns a list of two elements:
the new email content as a string, and a list of attachments to be added
to the end of the mail."
  (let ((processed-results (remove-and-get-inline-mail-content new-content))
        (generate-quoted-html-local-yank local-yank))
    (unless generate-quoted-html-local-yank
      (error "local-yank is nil"))
    (with-temp-files ((new-message "email-new")
                      (old-message "email-old"))
      (with-temp-buffer
        (insert (car processed-results))
        (muse-publish-markup-buffer nil "html")
        (write-file new-message))
      (with-temp-buffer
        (gnus-request-article (car generate-quoted-html-local-yank)
                              (cadr generate-quoted-html-local-yank)
                              (current-buffer))
        (write-file old-message))
      (with-temp-buffer
        (let ((error-buffer (get-buffer-create "*format-quoted-email errors*")))
          (unless (zerop (shell-command (format "%s \"%s\" \"%s\" /tmp"
                                                outlook-format-program
                                                new-message old-message)
                                        (current-buffer) error-buffer))
            (switch-to-buffer error-buffer)
            (error "Formatting of email failed")))
        
        (goto-char (point-min))
        (let ((images (loop while (not (looking-at "==END=="))
                            collect (get-next-pair))))
          (forward-line) ; Skip past the ==END== marker so it gets deleted too
          (when (= (point) (point-max))
            (error "Reached the end of the buffer while processing attachments"))
          (delete-region (point-min) (point))

          (list (buffer-string)
                (append (cadr processed-results) (mapcar #'car images))
                (mapcar #'cadr images)))))))

(defun simple-muse-message (content)
  (let ((processed-results (remove-and-get-inline-mail-content content)))
    (with-temp-buffer
      (insert (car processed-results))
      (muse-publish-markup-buffer nil "html")
      (list (buffer-string) (cadr processed-results) nil))))

(defun remove-trailing-newline (s)
  (loop for i from (1- (length s)) downto 0
        while (member (aref s i) '(?\n ?\r))
        finally (return (subseq s 0 (1+ i)))))

(defun call-muse-for-message ()
  (interactive)
  (save-excursion
    (message-goto-body)
    (when (looking-at (concat email-muse-format-marker "\n"))
      (replace-match "")
      (let* ((orig-content (buffer-substring (point) (point-max)))
             (parts (split-string orig-content (concat "^" mail-message-divider "$")))
             (length (length parts))
             (processed-results (cond ((= length 1)
                                       (simple-muse-message (car parts)))
                                      ((= length 2)
                                       (generate-quoted-html (car parts)))
                                      (t
                                       (error "Split resulted in %d parts" length))))
             (attachments (cadr processed-results)))
        (destructuring-bind (content attachments files-to-delete) processed-results
          (delete-region (point) (point-max))
          (insert "<#multipart type=alternative>\n")
          (insert (remove-inline-mail-content (car parts)))
          (when (cadr parts)
            (insert "\n===============================\n")
            (insert (remove-inline-mail-content (cadr parts)) "\n"))
          (when attachments
            (insert "<#multipart type=related>\n"))
          (insert "<#part type=text/html>\n")
          (insert content "\n")
          (when attachments
            (dolist (attachment attachments)
              (insert attachment "\n"))
            (insert "<#/multipart>\n"))
          (insert "<#/multipart>\n")

          ;; If there are files to be deleted, add them to the buffer-local list
          (when files-to-delete
            (set (make-local-variable 'local-temporary-files) files-to-delete)))))))
          
(defun mail-insert-divider ()
  (unless (save-excursion (message-goto-body) (search-forward "<#mml" nil t))
    (let ((replyp (save-excursion
                    (message-goto-body)
                    (cond ((= (point) (point-max))
                           nil)
                          (t
                           (insert "\n\n" mail-message-divider "\n")
                           (replace-regexp "^> ?\\(.*\\)$" "\\1" nil (point) (point-max))
                           t)))))
      (message-goto-body)
      (insert email-muse-format-marker "\n")

      (let ((id gnus-message-group-art)
            (message (let ((v (car yank)))
                       (etypecase v
                         (number v)
                         (list (car v))))))
        (unless (or id message)
          (error (format ("failed to find yank: %s" yank))))
        (set (make-local-variable 'local-yank) (list message (car id))))

      (unless replyp
        (message-goto-to)))))

(defun cleanup-temporary-attachments ()
  (when (boundp 'local-temporary-files)
    (dolist (f local-temporary-files)
      (delete-file f))))

(add-hook 'gnus-message-setup-hook 'mail-insert-divider)
(add-hook 'message-send-hook 'call-muse-for-message)
(add-hook 'message-sent-hook 'cleanup-temporary-attachments)
