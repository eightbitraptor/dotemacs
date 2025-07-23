;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Performance optimizations - set these as early as possible
(setq gc-cons-threshold (* 50 1000 1000)) ; 50MB during init
(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Disable expensive UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; Disable file handlers during init for faster startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore file handlers and GC settings after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold (* 2 1000 1000)) ; 2MB after init
            (setq gc-cons-percentage 0.1)
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

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
(setq native-comp-jit-compilation t) ; Use the modern variable name

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

;; Configure use-package to not fail on missing packages during init
(setq use-package-always-ensure nil)
(setq use-package-always-defer nil)

;; Force rebuild of autoloads on first run if needed
(when (not (file-exists-p (expand-file-name "straight/build-cache.el" user-emacs-directory)))
  (straight-rebuild-all))

(use-package org)

;; Generate main init file with error handling and conditional tangling
(let ((readme-path (expand-file-name "README.org" user-emacs-directory))
      (init-path (expand-file-name "init.el" user-emacs-directory))
      (init-elc-path (expand-file-name "init.elc" user-emacs-directory)))
  (condition-case err
      (when (or (not (file-exists-p init-path))
                (file-newer-than-file-p readme-path init-path))
        (message "Tangling README.org...")
        (org-babel-tangle-file readme-path init-path)
        ;; Remove any old byte-compiled file when we retangle
        (when (file-exists-p init-elc-path)
          (delete-file init-elc-path)))
    (error (message "Failed to tangle README.org: %s" err)))
  ;; Load init.el (not init.elc) to avoid issues with missing packages
  (when (file-exists-p init-path)
    (load init-path nil nil t)))
