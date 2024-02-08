(defun my-append-env-var (var-name value)
  "Append VALUE to the beginning of current value of env variable VAR-NAME."
  (setenv var-name (if (getenv var-name)
                       (format "%s:%s" value (getenv var-name))
                     value)))
;; Set up library locations for native compilation on macOS
(let ((gccjitpath "/opt/homebrew/lib/gcc/11:/opt/homebrew/lib"))
  (mapc (lambda (var-name) (my-append-env-var var-name gccjitpath))
        '("LIBRARY_PATH" "LD_LIBRARY_PATH" "PATH")))

;; Disable native-comp error/warning reporting
(setq native-comp-async-report-warnings-errors nil)

;; Switch to the most updated version of org as early as possible
(add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/org"))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Designate customization file
(setq custom-file (expand-file-name "customisations.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package org)
;; Load main init file
(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))
