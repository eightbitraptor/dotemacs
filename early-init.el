;; Switch to the most updated version of org as early as possible
;;(add-to-list 'load-path (expand-file-name "~/Uniconfig/Settings/Emacs/straight/build/org"))

;; Configure Emacs directories
;;(setq user-emacs-directory (expand-file-name "~/Uniconfig/Settings/Emacs/"))
;;(add-to-list 'custom-theme-load-path "~/.config/base16/el/")

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

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
(org-babel-load-file (expand-file-name "init.org" user-emacs-directory))
