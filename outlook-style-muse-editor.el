(defun outlook-style--find-muse-message-boundaries ()
  (save-excursion
    (search-forward (regexp-quote (concat outlook-style-email-muse-format-marker "\n")) nil t)
    (let ((start (point))
          (end (if (search-forward (regexp-quote outlook-style-mail-message-divider) nil t)
                   (- (point) (length outlook-style-mail-message-divider))
                 (point-max))))
      (list start end))))

(defun outlook-style-save-muse-mode-buffer ()
  (interactive)
  (unless (boundp 'local-muse-mode-mail-buffer)
    (error "Not in local muse edit mode"))
  (let ((content (buffer-string))
        (buffer (current-buffer)))
    (switch-to-buffer local-muse-mode-mail-buffer)
    (let ((boundaries (outlook-style--find-muse-message-boundaries)))
      (unless boundaries
        (error "Unable to find message boundaries"))
      (delete-region (car boundaries) (cadr boundaries))
      (goto-char (car boundaries))
      (insert content)
      (goto-char (car boundaries))
      (kill-buffer buffer))))

(defun edit-mail-in-muse-mode ()
  (interactive)
  (message-goto-body)
  (let ((boundaries (outlook-style--find-muse-message-boundaries)))
    (if boundaries
        (let* ((mail-buffer (current-buffer))
               (content (buffer-substring (car boundaries) (cadr boundaries)))
               (buffer (if (boundp 'local-muse-buffer)
                           local-muse-buffer
                         (set (make-local-variable 'local-muse-buffer)
                              (generate-new-buffer "*Muse Edit Mail*")))))
          (switch-to-buffer buffer)
          (insert content)
          (goto-char (point-min))
          (muse-mode)
          (local-set-key (kbd "C-c C-c") 'outlook-style-save-muse-mode-buffer)
          (set (make-local-variable 'local-muse-mode-mail-buffer) mail-buffer))
      (message "Muse format marker not found"))))

(add-hook 'gnus-message-setup-hook
          (lambda ()
            (local-set-key (kbd "C-c C-e") 'edit-mail-in-muse-mode)))

(provide 'outlook-style-muse-editor)
