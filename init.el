;;; Package system

(require 'package)
(package-initialize)

(setq package-enable-at-startup nil
      package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities '(("melpa"        . 30)
                                   ("melpa-stable" . 20)
                                   ("org"          . 10)
                                   ("gnu"          . 0)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

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

(use-package night-owl-theme
  :ensure t
  :init (load-theme 'night-owl t))

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
(cond ((eq (window-system) 'x)  (set-face-attribute 'default nil :font "Jetbrains Mono 16"))
      ((eq (window-system) 'mac)
       (setq mac-frame-tabbing nil)
       (set-face-attribute 'default nil :font "Consolas 20")))

(setq native-comp-deferred-compilation t)

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
  "Kill other buffers that are associated with files."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove nil (mapcar #'(lambda (b) (when (buffer-file-name b) b))
                                  (buffer-list))))))

(defun open-line-above ()
  "Open a line above the line the point is at. Then move to that line and indent according to mode"
  (interactive)
  (indent-according-to-mode)
  (move-beginning-of-line 1)
  (newline)
  (previous-line)
  (indent-according-to-mode))
(global-set-key (kbd "C-o") 'open-line-above)

(defun duplicate-line (arg)
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
  (next-line arg))
(global-set-key (kbd "C-d") 'duplicate-line)

(defun my-alter-frame-font-size (fn)
  (let* ((current-font-name (frame-parameter nil 'font))
         (decomposed-font-name (x-decompose-font-name current-font-name))
         (font-size (string-to-number (aref decomposed-font-name 5))))
    (aset decomposed-font-name 5 (number-to-string (funcall fn font-size)))
    (set-frame-font (x-compose-font-name decomposed-font-name))))

(defun my-inc-frame-font-size ()
  (interactive)
  (my-alter-frame-font-size '1+))

(defun my-dec-frame-font-size ()
  (interactive)
  (my-alter-frame-font-size '1-))

(global-set-key (kbd "C-+") 'my-inc-frame-font-size)
(global-set-key (kbd "C-=") 'my-inc-frame-font-size)
(global-set-key (kbd "C--") 'my-dec-frame-font-size)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<up>")    'enlarge-window)
(global-set-key (kbd "s-<down>")  'shrink-window)

(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

(setq-default indent-tabs-mode nil)

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
(use-package crux
    :ensure t
    :bind (("C-a" . crux-move-beginning-of-line)))
(use-package flx :ensure t)
(use-package ivy-rich :ensure t)

(use-package ivy
  :ensure t
  :init (setq ivy-use-virtual-buffers t
              ivy-height 20)
        (ivy-mode 1)
  :config (ivy-rich-mode 1))

(use-package ivy-xref
  :ensure t
  :init (when (>= emacs-major-version 27)
          (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package imenu-list
  :ensure t
  :bind ("C-c C-t" . imenu-list-smart-toggle))

;; Projectile spins trying to calculate what to write in the modeline when using TRAMP.
;; forcing a static modeline causes tramp mode to get fast again
(use-package projectile
  :ensure t
  :config (setq projectile-dynamic-mode-line nil)
          (projectile-global-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init (setq projectile-completion-system 'ivy))

(use-package projectile-rails
  :ensure t
  :config (projectile-rails-global-mode t))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;; Code utilities (completion, whitespace management, Git etc)

(use-package direnv
  :ensure t
  :init (direnv-mode))

(use-package editorconfig
  :ensure t
  :init (editorconfig-mode 1))

(use-package ws-butler
  :ensure t
  :init (setq ws-butler-keep-whitespace-before-point nil)
  (ws-butler-global-mode))

(use-package company
  :ensure t
  :init (setq company-dabbrev-downcase 0)
        (setq company-idle-delay 0)
  :config (global-company-mode))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(setq vc-follow-symlinks t)
(use-package magit
  :ensure t
  :init (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
        (setq magit-push-current-set-remote-if-missing nil)
  :bind ("C-c s" . magit-status))


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
  :config (setq ruby-insert-encoding-magic-comment nil
                enh-ruby-add-encoding-comment-on-save nil
                enh-ruby-bounce-deep-indent t
                enh-ruby-deep-indent-construct nil
                enh-ruby-hanging-brace-indent-level 2
                case-fold-search t))

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

(setq lsp-client-packages '(lsp-solargraph lsp-clangd lsp-rust-analyzer))

(use-package lsp-mode
  :ensure t
  :config (setq lsp-idle-delay 0.1
                lsp-headerline-breadcrumb-enable nil
                lsp-rust-analyzer-cargo-watch-command "clippy"
                lsp-rust-analyzer-server-display-inlay-hints t)
          (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (rustic-mode . lsp)
         (enh-ruby-mode . lsp))
  :after (which-key)
  :bind-keymap ("M-l" . lsp-command-map))

(use-package lsp-ivy
  :ensure t
  :bind ("M-t" . 'lsp-ivy-workspace-symbol)
  :config (advice-add 'lsp-ivy--goto-symbol :before
                      (lambda (arg)
                        (xref-push-marker-stack))))

(use-package lsp-ui
  :ensure t
  :config (setq lsp-ui-sideline-mode nil
                lsp-ui-flycheck-live-reporting nil
                lsp-ui-sideline-enable nil
                lsp-ui-sideline-show-diagnostics nil)
  :bind (:map
         lsp-ui-mode-map
         ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . #'lsp-ui-peek-find-references)))

(use-package lsp-treemacs
  :ensure t
  :init (lsp-treemacs-sync-mode 1))

;;; Language: Rust

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config (setq lsp-eldoc-hook nil
                lsp-enable-symbol-highlighting nil
                lsp-signature-auto-activate nil
                rustic-format-on-save nil)
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
  :init (setq web-mode-markup-indent-offset 4)
        (setq web-mode-css-indent-offset 4)
        (setq web-mode-code-indent-offset 4)
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

(use-package org-static-blog
  :ensure t
  :init
  (setq org-static-blog-use-preview t
        org-static-blog-preview-convert-titles t
        org-static-blog-preview-ellipsis "..."
        org-static-blog-enable-tags t
        org-static-blog-publish-url "http://localhost:9090/"
        org-static-blog-publish-title "eightbitraptor.com"
        org-static-blog-posts-directory "~/src/org-blog/org/posts"
        org-static-blog-drafts-directory "~/src/org-blog/org/drafts/"
        org-static-blog-publish-directory "~/src/org-blog/")

  (setq org-static-blog-page-header
        (concat
         "<meta name=\"author\" content=\"eightbitraptor\">"
         "<meta name=\"referrer\" content=\"no-referrer\">"
         "<link href= \"/static/style.css\" rel=\"stylesheet\"
                type=\"text/css\" />"
         "<link rel=\"icon\" href=\"static/favicon.ico\">")

        org-static-blog-page-preamble
        (concat
         "<div class=\"header\">"
         "  <a href=\"https://www.eightbitraptor.com\">eightbitraptor.com</a>"
         "  <div class=\"sitelinks\">"
         "    <a href=\"/blog/about.html\">about</a>"
         "    | <a href=\"/blog/software.html\">software</a>"
         "    | <a href=\"/blog/archive.html\">archive</a>"
         "    | <a href=\"/blog/rss.xml\">rss</a>"
         "  </div>"
         "</div>")))
