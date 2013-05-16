(defun outlook-style--find-muse-message-boundaries ()
  (save-excursion
    (message-goto-body)
    (let ((start (point)))
      (unless (search-forward (regexp-quote outlook-style-conf-start) nil t)
        (error "Buffer is not an outlook-style buffer"))
      (forward-line 0)
      (list start (point)))))

;;;###autoload
(defun outlook-style-save-muse-mode-buffer ()
  "Copy the content of a Muse edit buffer and save it back to the original
email buffer."
  (interactive)
  (unless (boundp 'outlook-style--local-muse-mode-mail-buffer)
    (error "Not in local muse edit mode"))
  (let ((content (buffer-string))
        (buffer (current-buffer)))
    (switch-to-buffer outlook-style--local-muse-mode-mail-buffer)
    (let ((boundaries (outlook-style--find-muse-message-boundaries)))
      (unless boundaries
        (error "Unable to find message boundaries"))
      (delete-region (car boundaries) (cadr boundaries))
      (goto-char (car boundaries))
      (insert content)
      (goto-char (car boundaries))
      (kill-buffer buffer))))

;;;###autoload
(defun outlook-style-edit-mail-in-muse-mode ()
  "Open a new buffer for editing an email in Muse mode."
  (interactive)
  (message-goto-body)
  (let ((boundaries (outlook-style--find-muse-message-boundaries)))
    (if boundaries
        (let* ((mail-buffer (current-buffer))
               (content (buffer-substring (car boundaries) (cadr boundaries)))
               (buffer (if (and (boundp 'outlook-style--local-muse-buffer)
                                (buffer-live-p outlook-style--local-muse-buffer))
                           outlook-style--local-muse-buffer
                         (set (make-local-variable 'outlook-style--local-muse-buffer)
                              (generate-new-buffer "*Muse Edit Mail*")))))
          (switch-to-buffer buffer)
          (insert content)
          (goto-char (point-min))
          (muse-mode)
          (local-set-key (kbd "C-c C-c") 'outlook-style-save-muse-mode-buffer)
          (set (make-local-variable 'outlook-style--local-muse-mode-mail-buffer) mail-buffer))
      (message "Muse format marker not found"))))

(add-hook 'outlook-style-init-hook
          (lambda ()
            (local-set-key (kbd "C-c C-e") 'outlook-style-edit-mail-in-muse-mode)))

(provide 'outlook-style-muse-editor)
