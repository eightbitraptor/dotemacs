;;; debug-ruby-mode.el --- Debug enh-ruby-mode issues

;; This file contains debugging functions to diagnose why enh-ruby-mode
;; isn't automatically activating for .rb files

(defun debug-enh-ruby-mode ()
  "Debug function to check enh-ruby-mode status and configuration."
  (interactive)
  (with-output-to-temp-buffer "*enh-ruby-mode-debug*"
    (princ "=== Enhanced Ruby Mode Debug Information ===\n\n")

    ;; Check if enh-ruby-mode is available as a function
    (princ "1. Checking if enh-ruby-mode function exists:\n")
    (if (fboundp 'enh-ruby-mode)
        (princ "   ✓ enh-ruby-mode function is available\n")
      (princ "   ✗ enh-ruby-mode function is NOT available\n"))
    (princ "\n")

    ;; Check if the package is loaded
    (princ "2. Checking if enh-ruby-mode feature is loaded:\n")
    (if (featurep 'enh-ruby-mode)
        (princ "   ✓ enh-ruby-mode feature is loaded\n")
      (princ "   ✗ enh-ruby-mode feature is NOT loaded\n"))
    (princ "\n")

    ;; Check auto-mode-alist entries for .rb files
    (princ "3. Current auto-mode-alist entries for .rb files:\n")
    (let ((rb-modes '()))
      (dolist (entry auto-mode-alist)
        (when (string-match-p "\\.rb" (car entry))
          (push entry rb-modes)))
      (if rb-modes
          (dolist (mode (reverse rb-modes))
            (princ (format "   %s -> %s\n" (car mode) (cdr mode))))
        (princ "   No entries found for .rb files\n")))
    (princ "\n")

    ;; Check if straight has the package
    (princ "4. Checking straight.el status for enh-ruby-mode:\n")
    (if (fboundp 'straight--installed-p)
        (if (straight--installed-p 'enh-ruby-mode)
            (princ "   ✓ enh-ruby-mode is installed by straight.el\n")
          (princ "   ✗ enh-ruby-mode is NOT installed by straight.el\n"))
      (princ "   ? straight.el not available\n"))
    (princ "\n")

    ;; Check major-mode-remap-alist
    (princ "5. Checking major-mode-remap-alist:\n")
    (if (boundp 'major-mode-remap-alist)
        (let ((ruby-remaps '()))
          (dolist (remap major-mode-remap-alist)
            (when (eq (car remap) 'ruby-mode)
              (push remap ruby-remaps)))
          (if ruby-remaps
              (dolist (remap ruby-remaps)
                (princ (format "   %s -> %s\n" (car remap) (cdr remap))))
            (princ "   No remapping found for ruby-mode\n")))
      (princ "   major-mode-remap-alist not bound\n"))
    (princ "\n")

    ;; Try to manually activate enh-ruby-mode
    (princ "6. Testing manual activation of enh-ruby-mode:\n")
    (condition-case err
        (progn
          (with-temp-buffer
            (insert "# Test Ruby code\nputs 'Hello, World!'")
            (enh-ruby-mode)
            (princ (format "   ✓ Successfully activated enh-ruby-mode\n"))
            (princ (format "   Current major mode: %s\n" major-mode))))
      (error
       (princ (format "   ✗ Error activating enh-ruby-mode: %s\n" err))))
    (princ "\n")

    ;; Check for ruby-mode
    (princ "7. Checking standard ruby-mode:\n")
    (if (fboundp 'ruby-mode)
        (princ "   ✓ ruby-mode function is available\n")
      (princ "   ✗ ruby-mode function is NOT available\n"))
    (princ "\n")

    ;; List all Ruby-related modes
    (princ "8. All Ruby-related modes found:\n")
    (let ((ruby-modes '()))
      (mapatoms (lambda (sym)
                  (when (and (fboundp sym)
                             (string-match-p "ruby.*mode" (symbol-name sym)))
                    (push sym ruby-modes))))
      (if ruby-modes
          (dolist (mode (sort ruby-modes (lambda (a b) (string< (symbol-name a) (symbol-name b)))))
            (princ (format "   - %s\n" mode)))
        (princ "   No Ruby-related modes found\n")))
    (princ "\n")

    ;; Check load-path for enh-ruby-mode
    (princ "9. Searching load-path for enh-ruby-mode:\n")
    (let ((found nil))
      (dolist (path load-path)
        (when (and (stringp path)
                   (file-exists-p path)
                   (or (file-exists-p (expand-file-name "enh-ruby-mode.el" path))
                       (file-exists-p (expand-file-name "enh-ruby-mode.elc" path))))
          (princ (format "   Found in: %s\n" path))
          (setq found t)))
      (unless found
        (princ "   enh-ruby-mode.el not found in load-path\n")))
    (princ "\n")

    ;; Final test: open test.rb and check mode
    (princ "10. Opening test.rb to check automatic mode:\n")
    (condition-case err
        (let ((buf (find-file-noselect "/Users/mattvh/.emacs.d/test.rb")))
          (with-current-buffer buf
            (princ (format "    Major mode: %s\n" major-mode))
            (princ (format "    Mode name: %s\n" mode-name)))
          (kill-buffer buf))
      (error
       (princ (format "    Error opening test.rb: %s\n" err))))))

;; Function to fix enh-ruby-mode setup
(defun fix-enh-ruby-mode ()
  "Attempt to fix enh-ruby-mode setup."
  (interactive)
  ;; Try to require the package
  (condition-case err
      (require 'enh-ruby-mode)
    (error
     (message "Failed to require enh-ruby-mode: %s" err)))

  ;; Add to auto-mode-alist
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rabl\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Thorfile\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . enh-ruby-mode))

  (message "Enhanced Ruby mode setup attempted. Run M-x debug-enh-ruby-mode to check status."))

(provide 'debug-ruby-mode)
;;; debug-ruby-mode.el ends here