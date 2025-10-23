;; Enable package quickstart (must be near the beginning)
(setq package-quickstart t)

;; Use UTF-8 everywhere
(set-language-environment    'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)

(load-file (concat user-emacs-directory "helpers.el"))

(use-package vterm
  :defer t
  :config (setq vterm-shell (or (getenv "SHELL") "/bin/bash")
                vterm-kill-buffer-on-exit t
                vterm-max-scrollback 10000))

(use-package no-littering)
(use-package fish-mode :defer t)
(use-package dirvish
  :defer t
  :init (dirvish-override-dired-mode))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

(use-package chruby)
(chruby-use "3.4.5")

(let ((backup-dir (expand-file-name "backup/" user-emacs-directory))
      (autosave-dir (expand-file-name "autosave/" user-emacs-directory)))
  (when (not (file-directory-p autosave-dir))
    (make-directory autosave-dir))
  (when (not (file-directory-p backup-dir))
    (make-directory backup-dir))
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir)))
  (setq backup-by-copying t))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package doom-themes
  :config (load-theme 'doom-moonlight :no-confirm))

(use-package nyan-mode
  :init (nyan-mode))

(setq ring-bell-function
    (lambda ()
      (let ((orig-fg (face-foreground 'mode-line)))
        (set-face-foreground 'mode-line "#FFFFFF")
        (run-with-idle-timer 0.1 nil
                             (lambda (fg) (set-face-foreground 'mode-line fg))
                             orig-fg))))

(setq-default cursor-type 'bar)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq-default line-spacing 5)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(cond ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "JetBrains Mono 14"))
      ((eq system-type 'darwin)
       (setq mac-frame-tabbing nil)
       (set-face-attribute 'default nil :font "Jetbrains Mono 16"))
      ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :font "Consolas 16")))

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(delete-selection-mode t)

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-o" . crux-smart-open-line-above)
         ("C-k" . crux-smart-kill-line)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)  ; Only in programming modes
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

(use-package ripgrep
  :defer t)

(use-package orderless
  :defer t
  :config
  ;; Configure completion styles for optimal performance
  (setq completion-styles '(orderless flex basic partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  ;; Configure orderless matching styles - optimized for performance
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))

  ;; Ensure completion updates as you type
  (setq orderless-component-separator "[ &]"))  ; Space or & as separators

;; vertico/consult/marginalia - search and browse in the minibuffer.
;; modern replacement for Ivy/Helm that uses better Emacs conventions
(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-count (if (display-graphic-p) 15 8)
        vertico-resize nil
        vertico-cycle t
        enable-recursive-minibuffers t
        max-mini-window-height 0.3
        resize-mini-windows t))

(use-package marginalia
  :config
  (marginalia-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq consult-preview-key "M-."
        consult-narrow-key "<"
        consult-project-function (lambda (_)
                                   (when (fboundp 'projectile-project-root)
                                     (projectile-project-root))))
  ;; Bind M-t to regular consult-imenu (now configured to be flat)
  (global-set-key (kbd "M-t") 'consult-imenu))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package imenu-list
  :bind ("C-c C-t" . imenu-list-smart-toggle))

(use-package treemacs
  :config (setq treemacs-width 35
                treemacs-click-mouse-1 'single-click)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (when (executable-find "git")
    (treemacs-git-mode 'deferred))
  (treemacs-resize-icons 18)

  :bind (("C-x t t" . treemacs)
         ("C-x t 0" . treemacs-select-window)))
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package projectile
  :config (setq projectile-dynamic-mode-line nil)
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("M-o" . projectile-find-file)
         ("C-S-g" . projectile-grep))
  :init (setq projectile-completion-system 'default))

(use-package projectile-rails
  :after projectile
  :hook (projectile-mode . projectile-rails-on))

(use-package projectile-ripgrep
  :after projectile)

(use-package direnv
  :if (executable-find "direnv")
  :init (direnv-mode))

(use-package editorconfig
  :init (editorconfig-mode 1))

(use-package company
  :hook (prog-mode . company-mode)  ; Only in programming modes
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 3
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-require-match nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-show-quick-access t
        company-selection-wrap-around t)

  (setq company-backends
        '((company-capf              ; Completion-at-point (includes LSP)
           company-dabbrev-code      ; Dynamic abbreviations from code
           company-keywords          ; Programming language keywords
           company-files             ; File path completion
           company-dabbrev)))        ; Dynamic abbreviations from all buffers

  (setq company-continue-commands t)         ; Continue showing popup while typing

  (setq company-tooltip-idle-delay 0.3
        company-async-timeout 5
        company-format-margin-function #'company-vscode-dark-icons-margin))

(setq vc-follow-symlinks t)
(declare-function magit-display-buffer-fullframe-status-v1 "magit")
(use-package magit
  :init (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-push-current-set-remote-if-missing nil)
  :bind ("C-c s" . magit-status))

(append straight-built-in-pseudo-packages '(xref project flycheck))
(require 'xref)

(advice-add 'isearch-forward :before (lambda (arg1 arg2) (xref-push-marker-stack)))
(advice-add 'isearch-backward :before (lambda (arg1 arg2) (xref-push-marker-stack)))

(use-package tree-sitter
  :hook ((c-mode . tree-sitter-mode)
         (c++-mode . tree-sitter-mode)
         (rust-mode . tree-sitter-mode)
         (rustic-mode . tree-sitter-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package eglot
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (enh-ruby-mode . eglot-ensure)
         (rustic-mode . eglot-ensure))
  :bind (("<mouse-4>" . xref-find-definitions)
         ("S-<down-mouse-1>" . xref-find-definitions)
         ("S-<down-mouse-2>" . xref-find-references)
         ("<mouse-5>" . xref-go-back)
         ("<f12>" . xref-find-references))
  :config
  (setq eglot-server-programs
        '(((c-mode c++-mode) .
                  ("clangd"
                   "--header-insertion=never"
                   "--enable-config"
                   "--all-scopes-completion"
                   "--background-index"))
          ((ruby-mode enh-ruby-mode) . ("ruby-lsp"))
          ((rust-mode rustic-mode) . ("rust-analyzer")))
        eglot-ignored-server-capabilities
        '(:documentOnTypeFormattingProvider
          :documentFormattingProvider
          :hoverProvider)
        eglot-sync-connect nil
        eglot-autoshutdown t
        xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package enh-ruby-mode
  :ensure t
  :defer nil  ; Force immediate loading
  :interpreter ("ruby" . enh-ruby-mode)
  :mode (("\\.rb\\'" . enh-ruby-mode)
         ("Gemfile\\'" . enh-ruby-mode)
         ("\\.ru\\'" . enh-ruby-mode)
         ("Rakefile\\'" . enh-ruby-mode)
         ("\\.rake\\'" . enh-ruby-mode))
  :hook (enh-ruby-mode . subword-mode)
  :init
  ;; Ensure the mode is available before auto-mode-alist is modified
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Enhanced Ruby Mode" t)
  :config
  (setq ruby-insert-encoding-magic-comment nil
        enh-ruby-add-encoding-comment-on-save nil
        enh-ruby-bounce-deep-indent t
        enh-ruby-deep-indent-construct nil
        enh-ruby-hanging-brace-indent-level 2
        case-fold-search t))

;; This really shouldn't be loaded all the time, only when we're working on Ruby
;;(let ((ruby-misc-dir "~/git/ruby/misc"))
;;  (if (file-directory-p ruby-misc-dir)
;;      (progn
;;        (add-to-list 'load-path ruby-misc-dir)
;;        (require 'ruby-style))))

(use-package meson-mode :defer t)

(use-package rustic
  :hook (rust-mode . rustic-mode)
  :config (setq lsp-enable-symbol-highlighting nil
                lsp-signature-auto-activate nil
                rustic-format-on-save nil))

(use-package ess
  :mode "\\.R"
  :defer t)

(use-package web-mode
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
        (setq web-mode-enable-auto-indentation 1))

(use-package zig-mode
  :defer t)

(setq markdown-preview-stylesheets
      (list "http://thomasf.github.io/solarized-css/solarized-dark.min.css"))

(use-package toml-mode
  :mode "\\.toml")

(use-package yaml-mode
  :mode "\\.yml"
        "\\.yaml")

(use-package markdown-mode
  :mode "\\.md"
        "\\.markdown")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-d") 'mvh/duplicate-line)
(global-set-key (kbd "C-+") 'mvh/inc-frame-font-size)
(global-set-key (kbd "C-=") 'mvh/inc-frame-font-size)
(global-set-key (kbd "C--") 'mvh/dec-frame-font-size)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<up>")    'enlarge-window)
(global-set-key (kbd "s-<down>")  'shrink-window)

(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(add-hook 'emacs-startup-hook #'mvh/display-startup-time)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))
