;;; Startup Time - START
(defvar *emacs-load-start* (current-time))

;;; Uncomment this in case of startup errors
;;;(setq debug-on-error t)

;;; Package system

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/")))

(setq package-archive-priorities '(("melpa"        . 30)
                                   ("melpa-stable" . 20)
                                   ("org"          . 10)
                                   ("gnu"          . 0)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Initialisation and Environment setup

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init (setq exec-path-from-shell-variables '("PATH"
                                               "MANPATH"
                                               "GEM_HOME"
                                               "GEM_PATH"))
        (setq exec-path-from-shell-check-startup-files nil)
        (exec-path-from-shell-initialize))

(use-package no-littering :ensure t)

;;; Themes and appearance

;;(use-package feebleline
;;    :ensure t
;;    :config (feebleline-mode t))

;; (use-package doom-themes
;;   :ensure t
;;   :init (load-theme 'modus-operandi t))

(use-package ample-theme
  :ensure t
  : init (load-theme 'ample-light t))

(use-package nyan-mode
  :ensure t
  :init (nyan-mode))
(setq visible-bell t)
(setq-default cursor-type 'bar)
(toggle-scroll-bar -1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq-default line-spacing 5)
(global-display-line-numbers-mode t)
(cond ((eq (window-system) 'x)  (set-face-attribute 'default nil :font "Liberation Mono 14"))
      ((eq (window-system) 'mac)
       (setq mac-frame-tabbing nil)
       (set-face-attribute 'default nil :font "Consolas 16")))

;;; General editor behaviour


(let ((mvh-customisations-file "~/.emacs.d/customisations.el"))
  (unless (file-exists-p mvh-customisations-file)
    (write-region "" nil mvh-customisations-file))
  (setq custom-file mvh-customisations-file)
  (load custom-file))

(setq auto-save-default nil)
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))
(setq make-backup-files nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

(fset 'yes-or-no-p 'y-or-n-p)

(when window-system
  ((lambda ()
     (global-unset-key "\C-z")
     (global-unset-key "\C-x\C-z"))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun open-line-above ()
  "Open a line above the line the point is at. Then move to that line and indent according to mode"
  (interactive)
  (indent-according-to-mode)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))
(global-set-key (kbd "C-o") 'open-line-above)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<up>")    'enlarge-window)
(global-set-key (kbd "s-<down>")  'shrink-window)

(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

(setq-default indent-tabs-mode nil)
(electric-indent-mode -1)

(setq-default c-basic-offset 4)

(delete-selection-mode t)

;;; Navigation and Search

(use-package which-key
  :ensure t
  :config (which-key-mode)
          (setq which-key-idle-delay 3))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package ag :ensure t)
(use-package counsel :ensure t)
(use-package crux
    :ensure t
    :bind (("C-a" . crux-move-beginning-of-line)))
(use-package flx :ensure t)
(use-package ivy-rich :ensure t)

(use-package ivy
  :ensure t
  :init (setq ivy-use-virtual-buffers t)
        (setq ivy-re-builders-alist
              '((swiper . ivy--regex-plus)
                (t      . ivy--regex-fuzzy)))
        (ivy-mode 1)
  :config (ivy-rich-mode 1)
  :bind ;;("C-s"     . swiper)
        ("M-x"     . counsel-M-x)
        ("<f2> i"  . counsel-info-lookup-symbol)
        ("C-c u "  . counsel-unicode-char)
        ("C-c j"   . counsel-git-grep)
        ("C-c k"   . counsel-ag))

(use-package ivy-hydra
  :ensure t)

(use-package ivy-xref
  :ensure t
  :init (when (>= emacs-major-version 27)
          (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package minimap
  :ensure t
  :config (setq minimap-window-location 'right)
          (setq minimap-automatically-delete-window nil)
  :bind ("C-c m" . minimap-mode))

(use-package project-explorer
  :ensure t
  :bind ("C-c C-p" . project-explorer-toggle))

(use-package imenu-list
  :ensure t
  :bind ("C-c C-t" . imenu-list-smart-toggle))

(defun mvh-projectile-switch-project ()
  "Load the environment from my local shell to capture env before finding a file."
  (exec-path-from-shell-initialize)
  (if (string= "shopify" (projectile-project-name))
      (progn
        (setq ivy-re-builders-alist
              '((swiper . ivy--regex-plus)
                (t      . ivy--regex-plus)))))
  (projectile-find-file))

(use-package projectile
  :ensure t
  :config (setq projectile-switch-project-action 'mvh-projectile-switch-project)
          (setq projectile-globally-ignored-directories
                (append '(".buildkite"
                          ".github"
                          ".bundle"
                          ".cache"
                          ".dev"
                          "tmp"
                          "log"
                          "*/images"
                          "/app/assets") projectile-globally-ignored-directories))


          (setq projectile-globally-ignored-files
                (append '(".DS_Store"
                          ".codecov.yml"
                          ".byebug_history"
                          ".image_optim.yml"
                          ".rubocop-http---shopify-github-io-ruby-style-guide-rubocop-yml"
                          ".rubocop.ci.yml"
                          ".rubocop.yml"
                          ".yardopts"
                          ".yarnclean"
                          ".eslintignore"
                          ".projectile") projectile-globally-ignored-files))

          (setq projectile-globally-ignored-file-suffixes
                (append '(".svg"
                          ".jpg"
                          ".png"
                          ".gif"
                          ".pdf"
                          ".woff"
                          ".woff2"
                          ".ttf"
                          ".eot"
                          ".js"
                          ".coffee"
                          ".zpl"
                          ".scss") projectile-globally-ignored-file-suffixes))
          (projectile-global-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-completion-system 'ivy))

(use-package projectile-rails
  :ensure t
  :config (projectile-rails-global-mode t))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;;; Code utilities (completion, whitespace management, Git etc)

(use-package company
  :ensure t
  :init (setq company-dabbrev-downcase 0)
        (setq company-idle-delay 0)
  :config (global-company-mode)
          (push 'company-robe company-backends))

(setq vc-follow-symlinks t)
(use-package magit
  :ensure t
  :init (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
        (setq magit-push-current-set-remote-if-missing nil)
  :bind ("C-c s" . magit-status))

(use-package ws-butler
  :ensure t
  :init (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :init (add-hook 'prog-mode-hook 'global-flycheck-mode)
  :config (progn
            ;; Settings
            (setq-default flycheck-highlighting-mode 'lines
                          flycheck-check-syntax-automatically '(save)
                          flycheck-disabled-checkers '(c/c++-clang c/c++-gcc ruby))))

(use-package yasnippet
  :ensure t
  :config (setq yas-verbosity 1
                yas-wrap-around-region t)
          (with-eval-after-load 'yasnippet
            (setq yas-snippet-dirs '(yasnippet-snippets-dir)))
           (yas-reload-all)
           (yas-global-mode))
(use-package yasnippet-snippets :ensure t)

;;; Language: Ruby

(defun ruby-frozen-string-literal ()
  "Check the current buffer for the magic comment # frozen_string_literal: true.
If the comment doesn't exist, offer to insert it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (string= (thing-at-point 'line)
                     "# frozen_string_literal: true\n")
      (insert "# frozen_string_literal: true\n\n"))))

(use-package robe :ensure t)
(use-package rbenv
  :ensure t
  :init (global-rbenv-mode)
        (rbenv-use-global))

(use-package minitest :ensure t)

(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb"
        "\\Gemfile"
        "\\.ru"
        "\\Rakefile"
        "\\.rake"
  :hook robe-mode
  :config (setq ruby-insert-encoding-magic-comment nil)
          (setq enh-ruby-add-encoding-comment-on-save nil)
          (setq enh-ruby-bounce-deep-indent t)
          (setq enh-ruby-deep-indent-construct nil)
          (setq flycheck-command-wrapper-function
            (lambda (command)
              (append '("bundle" "exec") command)))
          (setq enh-ruby-hanging-brace-indent-level 2))

;;; Language: C

;;; the Ruby core team maintain an c-mode style specifically for the
;;; MRI source code, let's use it if we have Ruby checked out.
(let ((ruby-misc-dir "~/src/ruby/misc"))
  (if (file-directory-p ruby-misc-dir)
      (progn
        (add-to-list 'load-path ruby-misc-dir)
        (require 'ruby-style)
        (use-package ruby-style))))

;;; Language Server

(use-package lsp-mode
  :ensure t
  :config (setq lsp-idle-delay 0.1
                lsp-headerline-breadcrumb-enable nil
                lsp-before-save-edits nil
                lsp-enable-on-type-formatting nil
                lsp-signature-auto-activate nil
                lsp-signature-render-documentation nil
                lsp-completion-enable-additional-text-edit nil
                lsp-completion-show-detail 1
                lsp-completion-show-kind 1
                company-minimum-prefix-length 1
                lsp-rust-analyzer-cargo-watch-command "clippy"
                lsp-eldoc-render-all nil
                lsp-lens-enable nil
                lsp-ui-doc-enable nil
                lsp-idle-delay 0.6
                lsp-rust-analyzer-server-display-inlay-hints t)
          (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (rustic-mode . lsp)
         (enh-ruby-mode . lsp))
  :init (yas-global-mode)
  :after (which-key)
  :bind-keymap ("M-l" . lsp-command-map))

(use-package lsp-ivy
  :ensure t
  :bind ("M-t" . 'lsp-ivy-workspace-symbol)
  :config (advice-add 'lsp-ivy--goto-symbol :before
                      (lambda (arg)
                        (xref-push-marker-stack))))

(use-package lsp-ui :ensure t)

;;; Language: Rust

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config (setq lsp-eldoc-hook nil)
          (setq lsp-enable-symbol-highlighting nil)
          (setq lsp-signature-auto-activate nil)
          (setq rustic-format-on-save nil)
          (add-hook 'rustic-mode-hook 'mvh/rustic-mode-hook))

(defun mvh/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but
  ;; don't try to save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved
  ;; this should no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

;;; Language: HTML/Web stuff

(use-package web-mode
  :ensure t
  :mode "\\.tsx"
        "\\.erb"
        "\\.jsx"
        "\\.html"
        "\\.css"
        "\\.scss"
        "\\.sass"
  :init (setq web-mode-markup-indent-offset 2)
        (setq web-mode-css-indent-offset 2)
        (setq web-mode-code-indent-offset 2)
        (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
        (setq web-mode-enable-auto-indentation nil))


;;; Language: Toml/Yaml/Markdown

(setq markdown-preview-stylesheets
      (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

(use-package toml-mode
  :ensure t
  :mode "\\.toml")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml"
        "\\.yaml")

(use-package markdown-mode
  :ensure t
  :mode "\\.md"
        "\\.markdown")


;;; Org Mode

(use-package htmlize :ensure t)
(use-package org
  :ensure t
  :config (setq org-startup-truncated 1)
          (add-hook 'org-shiftup-final-hook 'windmove-up)
          (add-hook 'org-shiftleft-final-hook 'windmove-left)
          (add-hook 'org-shiftdown-final-hook 'windmove-down)
          (add-hook 'org-shiftright-final-hook 'windmove-right)
          (org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)))
  :mode ("\\.org" . org-mode))

(use-package org-journal
  :ensure t
  :init (setq org-journal-prefix-key "C-c j ")
  :custom (org-journal-dir "~/Documents/log_books/")
          (org-journal-file-format "%Y%m%d")
          (org-journal-date-format "%A %d %b %Y"))

  ;; Server
  ;;(unless (bound-and-true-p server-running-p)
  ;; (server-start))

;; Startup Time - END
(message "My .emacs loaded in %ds" (cl-destructuring-bind
                                       (hi lo ms psec)
                                       (current-time)
                                     (- (+ hi lo)
                                        (+ (cl-first *emacs-load-start*)
                                           (cl-second *emacs-load-start*)))))
