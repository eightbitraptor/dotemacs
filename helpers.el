(defun mvh/kill-other-buffers ()
  "Make the current buffer the only focus, and kill other buffers
that are associated with files."
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove nil (mapcar #'(lambda (b) (when (buffer-file-name b) b))
                                  (buffer-list))))))

(defun mvh/duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (forward-line arg))

(defun mvh/alter-frame-font-size (fn)
  (when (display-graphic-p)
    (let* ((current-font-name (frame-parameter nil 'font))
           (decomposed-font-name (x-decompose-font-name current-font-name))
           (font-size (string-to-number (aref decomposed-font-name 5))))
      (aset decomposed-font-name 5 (number-to-string (funcall fn font-size)))
      (set-frame-font (x-compose-font-name decomposed-font-name)))))

(defun mvh/inc-frame-font-size ()
  (interactive)
  (mvh/alter-frame-font-size '1+))

(defun mvh/dec-frame-font-size ()
  (interactive)
  (mvh/alter-frame-font-size '1-))

(defun mvh/ruby-frozen-string-literal ()
  "Check the current buffer for the magic comment # frozen_string_literal: true.
If the comment doesn't exist, offer to insert it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (string= (thing-at-point 'line)
                     "# frozen_string_literal: true\n")
      (insert "# frozen_string_literal: true\n\n"))))

(defun mvh/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))
