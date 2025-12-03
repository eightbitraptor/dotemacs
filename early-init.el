;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Enable native-compilation
(setq native-comp-jit-compilation t)
(setq native-comp-async-report-warnings-errors nil)
;; max optimisations
(setq native-comp-speed 2)

;; Performance optimizations - set these as early as possible
(setq gc-cons-threshold (* 50 1000 1000)) ; 50MB during init
(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Disable expensive UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; Transparency for some eyecandy
(push '(alpha . 95) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise       t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil
      package-quickstart nil)

;; macOS-specific UI settings
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

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
