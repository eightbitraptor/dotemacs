;; Enable package quickstart (must be near the beginning)
(setq package-quickstart t)

;; Ensure straight.el packages can be found
;;(eval-when-compile
;;  (add-to-list 'load-path (expand-file-name "straight/build/" user-emacs-directory)))

;; Manual Tree-sitter configuration for better performance and control
(when (treesit-available-p)
  ;; Define grammar sources for languages where Tree-sitter excels
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")))

  ;; Use Tree-sitter modes for C/C++ and Rust only
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (rust-mode . rust-ts-mode)))

  ;; Note: Run M-x treesit-install-language-grammar for each language
  ;; on first use to compile the grammar libraries
  )

;; Cache for exec-path-from-shell to avoid repeated initialization
(defvar mvh/exec-path-initialized nil
  "Whether exec-path-from-shell has been initialized.")

(use-package exec-path-from-shell
  :if (memq system-type '(darwin gnu/linux))
  :defer t  ; Don't load immediately
  :config
  (setq exec-path-from-shell-variables '("PATH"
                                        "MANPATH"
                                        "GEM_HOME"
                                        "GEM_PATH"
                                        "RUBY_ROOT"
                                        "RUBY_ENGINE"
                                        "RUBY_VERSION"))
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments '("-l"))  ; Use login shell for proper environment
  :commands (exec-path-from-shell-initialize))

;; Initialize exec-path lazily when needed for Ruby/programming modes
(defun mvh/ensure-exec-path ()
  "Initialize exec-path from shell if not already done."
  (unless mvh/exec-path-initialized
    (require 'exec-path-from-shell nil t)
    (when (fboundp 'exec-path-from-shell-initialize)
      (exec-path-from-shell-initialize)
      (setq mvh/exec-path-initialized t))))

;; Hook into modes that need proper PATH
(add-hook 'prog-mode-hook #'mvh/ensure-exec-path)

(setq frame-resize-pixelwise t)

(use-package vterm
  :defer t
  :config
  ;; Set shell to use
  (setq vterm-shell (or (getenv "SHELL") "/bin/bash"))
  ;; Kill buffer when process exits
  (setq vterm-kill-buffer-on-exit t)
  ;; Increase scrollback
  (setq vterm-max-scrollback 10000))

(use-package no-littering)

(use-package fish-mode)

(use-package dirvish
  :init (dirvish-override-dired-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-mode)
  :init
  ;; Enable caching for better performance
  (setq nerd-icons-completion-cache t)
  :config

  ;; Add icon support for imenu items with caching
  (defvar nerd-icons-imenu-icon-cache nil
    "Cached icon string for imenu candidates.")

  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql imenu)))
    "Return cached icon for imenu candidates."
    (or nerd-icons-imenu-icon-cache
        (setq nerd-icons-imenu-icon-cache
              (concat (nerd-icons-codicon "nf-cod-symbol_method"
                                          :face 'font-lock-function-name-face
                                          :height (or nerd-icons-completion-icon-size 1.0))
                      " ")))))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  ;; Enable caching for better performance
  (setq nerd-icons-dired-cache t))

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
(when (display-graphic-p)
  (toggle-scroll-bar -1)
  (tool-bar-mode 0))
(menu-bar-mode 0)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq-default line-spacing 5)
;; Enable line numbers only in programming modes for better performance
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(cond ((eq system-type 'gnu/linux)
       (set-face-attribute 'default nil :font "JetBrains Mono 14"))
      ((eq system-type 'darwin)
       (setq mac-frame-tabbing nil)
       (set-face-attribute 'default nil :font "Jetbrains Mono 16"))
      ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :font "Consolas 16")))

;; Set initial frame size and position

(defun mvh/set-initial-frame ()
  (when (display-graphic-p)
    (let* ((base-factor 0.85)
           (primary-monitor-actual-width
            (nth 3 (assoc 'geometry (car (display-monitor-attributes-list)))))
         (a-width (* primary-monitor-actual-width base-factor))
         (a-height (* (display-pixel-height) base-factor))
         (a-left (truncate (/ (- primary-monitor-actual-width a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
      (set-frame-position (selected-frame) a-left a-top)
      (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t))))
(mvh/set-initial-frame)

(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key (kbd "C-u") 'unfill-paragraph)

(global-display-fill-column-indicator-mode)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

;; Disabled beacon-mode for better performance
;; (use-package beacon
;;   :config (beacon-mode 1))

(fset 'yes-or-no-p 'y-or-n-p)

(when window-system
  ((lambda ()
     (global-unset-key "\C-z")
     (global-unset-key "\C-x\C-z"))))

(global-auto-revert-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

(delete-selection-mode t)

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;; GC settings moved to early-init.el for better startup performance
;; Runtime values are set in emacs-startup-hook

(use-package undo-tree
  :init (setq undo-tree-auto-save-history nil
              undo-tree-history-directory-alist '("~/.emacs.d/autosave/"))

  :config (global-undo-tree-mode))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-o" . crux-smart-open-line-above)
         ("C-k" . crux-smart-kill-line)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)  ; Only in programming modes
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

(defun kill-other-buffers ()
  "Make the current buffer the only focus, and kill other buffers
that are associated with files."
  (interactive)
  (delete-other-windows)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove nil (mapcar #'(lambda (b) (when (buffer-file-name b) b))
                                  (buffer-list))))))

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
  (forward-line arg))

(defun my-alter-frame-font-size (fn)
  (when (display-graphic-p)
    (let* ((current-font-name (frame-parameter nil 'font))
           (decomposed-font-name (x-decompose-font-name current-font-name))
           (font-size (string-to-number (aref decomposed-font-name 5))))
      (aset decomposed-font-name 5 (number-to-string (funcall fn font-size)))
      (set-frame-font (x-compose-font-name decomposed-font-name)))))

(defun my-inc-frame-font-size ()
  (interactive)
  (my-alter-frame-font-size '1+))

(defun my-dec-frame-font-size ()
  (interactive)
  (my-alter-frame-font-size '1-))

(global-set-key (kbd "C-d") 'duplicate-line)
(global-set-key (kbd "C-+") 'my-inc-frame-font-size)
(global-set-key (kbd "C-=") 'my-inc-frame-font-size)
(global-set-key (kbd "C--") 'my-dec-frame-font-size)

;; Standard xref navigation bindings
(global-set-key (kbd "M-.") 'xref-find-definitions)     ; Go to definition
(global-set-key (kbd "M-,") 'xref-go-back)             ; Go back in stack
(global-set-key (kbd "M-?") 'xref-find-references)     ; Find references
(global-set-key (kbd "C-M-.") 'xref-find-apropos)      ; Find symbols matching pattern

;; Additional navigation helpers
(global-set-key (kbd "C-M-,") 'xref-go-forward)        ; Go forward in stack (if available)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "s-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<up>")    'enlarge-window)
(global-set-key (kbd "s-<down>")  'shrink-window)

(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)

(global-set-key (kbd "M-∑") 'delete-frame)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package which-key
  :defer 1  ; Load 1 second after startup
  :config
  (which-key-mode)
  (setq which-key-idle-delay 3)

  ;; Add descriptions for custom commands
  (which-key-add-key-based-replacements
    "C-d" "duplicate line"
    "C-u" "unfill paragraph"
    "C-+" "increase font size"
    "C-=" "increase font size"
    "C--" "decrease font size"
    "M-t" "find symbol in buffer"
    "M-T" "find symbol in project"
    "M-o" "find file in project"
    "C-S-g" "grep project"
    "M-∑" "delete frame")

  ;; Group C-c commands
  (which-key-add-key-based-replacements
    "C-c h" "consult history"
    "C-c m" "consult mode command"
    "C-c k" "consult kmacro"
    "C-c s" "magit status"
    "C-c C-t" "toggle imenu sidebar"
    "C-c p" "projectile"
    "C-c l" "lsp"
    "C-c j" "journal")

  ;; Group window management commands
  (which-key-add-key-based-replacements
    "s-<left>" "shrink window width"
    "s-<right>" "enlarge window width"
    "s-<up>" "enlarge window height"
    "s-<down>" "shrink window height"
    "s-<return>" "toggle fullscreen"
    "s-'" "go back (xref)")

  ;; Navigation and search groups
  (which-key-add-key-based-replacements
    "M-g" "goto"
    "M-s" "search"
    "C-x r" "registers/rectangles")

  ;; Embark actions
  (which-key-add-key-based-replacements
    "C-." "embark act"
    "C-;" "embark dwim"
    "C-h B" "embark bindings")

  ;; Execution commands
  (which-key-add-key-based-replacements
    "C-x C-m" "execute command (M-x)"
    "C-c C-m" "execute command (M-x)"
    "C-x C-n" "create note (denote)"))

(use-package ripgrep
  :defer t)

(use-package orderless
  :demand t
  :config
  ;; Configure completion styles for optimal performance
  (setq completion-styles '(orderless flex basic partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))

  ;; Configure orderless matching styles - optimized for performance
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))

  ;; Ensure completion updates as you type
  (setq orderless-component-separator "[ &]"))  ; Space or & as separators

(use-package vertico
  :demand t
  :config
  (vertico-mode)

  ;; Performance and UX optimizations
  (setq vertico-count (if (display-graphic-p) 15 8)  ; Fewer lines in terminal
        vertico-resize nil        ; Keep minibuffer height fixed
        vertico-cycle t)          ; Cycle through completions

  ;; Enable recursive minibuffers for complex operations
  (setq enable-recursive-minibuffers t)

  ;; Control minibuffer window height
  (setq max-mini-window-height 0.3)  ; Max 30% of frame height
  (setq resize-mini-windows t))       ; Allow resizing but respect max

(use-package marginalia
  :demand t
  :config
  (marginalia-mode)

  ;; Bind M-A to cycle annotation levels in minibuffer
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

  ;; Enable nerd-icons integration
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package consult
  :demand t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; M-t bound separately for flattened imenu
         )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; Configure preview and async operations
  (setq consult-preview-key "M-."
        consult-narrow-key "<"
        consult-project-function (lambda (_)
                                   (when (fboundp 'projectile-project-root)
                                     (projectile-project-root))))

  ;; Configure async operations for performance
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")

  ;; Configure consult-imenu to be completely flat - no grouping or toplevel categories
  (setq consult-imenu-config
        '((t :toplevel nil :types nil)))  ; No toplevel grouping, no type categories

  ;; Bind M-t to regular consult-imenu (now configured to be flat)
  (global-set-key (kbd "M-t") 'consult-imenu))

(use-package embark
  :defer t
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
  :defer t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package imenu-list
  :bind ("C-c C-t" . imenu-list-smart-toggle))

(use-package treemacs
  :defer t
  :init
  ;; Add and expand projects automatically when visiting files
  (setq treemacs-project-follow-mode t
        treemacs-follow-mode t)
  :config
  ;; Basic settings - only what we need to change from defaults
  (setq treemacs-width 35
        treemacs-click-mouse-1 'single-click)  ; Single click to expand/collapse

  ;; Enable modes for auto-behavior
  (treemacs-follow-mode t)              ; Follow current buffer's file
  (treemacs-project-follow-mode t)      ; Auto-add projects when opening files
  (treemacs-filewatch-mode t)           ; Auto-refresh on file changes
  (treemacs-fringe-indicator-mode t)    ; Show position indicator

  ;; Enable git colors if available
  (when (executable-find "git")
    (treemacs-git-mode 'deferred))

  ;; Use nerd-icons since we already have it
  (treemacs-resize-icons 18)

  :bind
  (("C-x t t" . treemacs)
   ("C-x t 0" . treemacs-select-window)))

;; Integration with projectile for project management
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Integration with magit for better git status display
(use-package treemacs-magit
  :after (treemacs magit))

;; Use nerd-icons instead of all-the-icons for consistency with your setup
(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

;; Projectile spins trying to calculate what to write in the modeline when using TRAMP.
;; forcing a static modeline causes tramp mode to get fast again
(use-package projectile
  :config (setq projectile-dynamic-mode-line nil)
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("M-o" . projectile-find-file)
         ("C-S-g" . projectile-grep))
  :init (setq projectile-completion-system 'default))

(use-package ag
  :defer t)

(use-package projectile-rails
  :defer t
  :config (projectile-rails-global-mode t))
(use-package projectile-ripgrep
  :defer t)

(use-package direnv
  :if (executable-find "direnv")
  :init (direnv-mode))

(use-package editorconfig
  :init (editorconfig-mode 1))

(use-package company
  :hook (prog-mode . company-mode)  ; Only in programming modes
  :config
  ;; Core performance and behavior settings
  (setq company-idle-delay 0.3              ; Small delay for better typing experience
        company-minimum-prefix-length 1     ; Show completions after just 1 character
        company-tooltip-limit 10            ; Limit shown candidates for performance
        company-tooltip-align-annotations t ; Align annotations on the right
        company-require-match nil           ; Allow free typing
        company-dabbrev-downcase nil        ; Preserve case in dabbrev completions
        company-dabbrev-ignore-case nil     ; Case-sensitive dabbrev
        company-show-quick-access t         ; Show quick-access numbers
        company-selection-wrap-around t)    ; Wrap around when cycling

  ;; Enable dynamic filtering - the key feature you want
  (setq company-backends
        '((company-capf              ; Completion-at-point (includes LSP)
           company-dabbrev-code      ; Dynamic abbreviations from code
           company-keywords          ; Programming language keywords
           company-files             ; File path completion
           company-dabbrev)))        ; Dynamic abbreviations from all buffers

  ;; Ensure completions update as you type
  (setq company-abort-on-unique-match nil   ; Don't abort on unique match
        company-continue-commands t)         ; Continue showing popup while typing

  ;; Key bindings for navigation
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "M-p") 'company-select-previous-or-abort)

  ;; Improve performance with large candidate lists
  (setq company-tooltip-idle-delay 0.3
        company-async-timeout 5)

  ;; Enable Company in specific modes where it might be disabled
  (dolist (hook '(eshell-mode-hook
                  shell-mode-hook))
    (add-hook hook 'company-mode)))

;; Use Company's built-in VSCode-style icons
(with-eval-after-load 'company
  ;; Enable VSCode-style text icons that work everywhere
  (setq company-format-margin-function #'company-vscode-dark-icons-margin))

;; Use Dabbrev with Company
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; Company terminal support for non-graphical Emacs
(when (not (display-graphic-p))
  (with-eval-after-load 'company
    ;; Disable company-box in terminal
    (setq company-box-enable nil)))

;; LSP integration with Company
;; Company automatically uses company-capf which LSP configures
(with-eval-after-load 'lsp-mode
  ;; Ensure Company is used for completions
  (setq lsp-completion-provider :capf)

  ;; Configure LSP to work well with Company's dynamic filtering
  (setq lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-completion-enable-additional-text-edit t
        lsp-enable-snippet t)

  ;; Make sure Company respects LSP's sorting
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (setq-local company-transformers nil))))

(setq vc-follow-symlinks t)
;; Declare function to avoid warnings
(declare-function magit-display-buffer-fullframe-status-v1 "magit")

(use-package magit
  :defer t
  :init (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-push-current-set-remote-if-missing nil)
  :bind ("C-c s" . magit-status))

(use-package flycheck)

    (use-package lsp-mode
      :commands (lsp lsp-deferred)
      :init
      (setq lsp-keymap-prefix "C-c l")
      :config
      (setq lsp-idle-delay 0.5  ; Balance between responsiveness and performance
                    lsp-headerline-breadcrumb-enable nil
                    lsp-enable-on-type-formatting nil
                    lsp-enable-indentation nil
                    lsp-solargraph-formatting nil
                    lsp-eldoc-enable-hover nil
                    lsp-enable-snippets nil
                    ;; Optimize completion settings
                    lsp-completion-provider :capf
                    lsp-completion-enable t
                    lsp-completion-show-detail t
                    lsp-completion-show-kind t
                    ;; Enable signature help for function parameters
                    lsp-signature-auto-activate t
                    lsp-signature-render-documentation nil  ; Keep signatures concise
                    ;; Language server specific settings
                    lsp-rust-analyzer-cargo-watch-command "clippy"
                    lsp-rust-analyzer-server-display-inlay-hints t
                    lsp-enabled-clients '(clangd
                                          ruby-lsp-ls
                                          rust-analyzer)
                    ;; Explicitly disable other Ruby language servers to suppress warnings
                    lsp-disabled-clients '(steep-ls
                                           solargraph
                                           sorbet-ls
                                           ruby-ls
                                           semgrep-ls
                                           ruby-syntax-tree-ls
                                           rubocop-ls
                                           typeprof-ls)
                    ;; Configure ruby-lsp to use the correct command
                    lsp-ruby-lsp-server-command '("ruby-lsp")
                    lsp-clients-clangd-args '("--header-insertion=never"
                                              "--enable-config"
                                              "--all-scopes-completion"
                                              "--background-index"))
      :hook ((c-mode . lsp-deferred)
             (c++-mode . lsp-deferred)
             (c-ts-mode . lsp-deferred)
             (c++-ts-mode . lsp-deferred)
             (ruby-mode . lsp-deferred)
             (enh-ruby-mode . lsp-deferred)
             (rustic-mode . lsp-deferred)
             (lsp-mode . lsp-enable-which-key-integration))
      :bind (("<mouse-4>" . lsp-find-definition)
             ("S-<down-mouse-1>" . lsp-find-definition)
             ("S-<down-mouse-2>" . lsp-find-references)
             ("<mouse-5>" . xref-go-back)
             ("<f12>" . lsp-find-references)
             ("s-'" . xref-go-back))
      :bind-keymap ("C-c l" . lsp-command-map))

  ;; Additional LSP helper functions (defined outside use-package for availability)
  (defun mvh/update-gem-path-for-chruby ()
    "Update PATH to include gem bin directories for the current Ruby.
This is necessary because chruby changes the Ruby executable but doesn't
automatically update Emacs' exec-path with the corresponding gem bin directories.
We add both the user gem directory (for gems installed with --user-install)
and the default gem directory (for regular gem installs)."
    (when (executable-find "ruby")
      (let* ((gem-info (shell-command-to-string "ruby -e 'puts Gem.user_dir; puts Gem.default_dir'"))
             (gem-lines (split-string gem-info "\n" t))
             (gem-user-dir (car gem-lines))
             (gem-default-dir (cadr gem-lines)))
        ;; Add user gem bin directory
        (when gem-user-dir
          (let ((user-bin (expand-file-name "bin" gem-user-dir)))
            (when (file-directory-p user-bin)
              (add-to-list 'exec-path user-bin)
              (setenv "PATH" (concat user-bin ":" (getenv "PATH"))))))
        ;; Add default gem bin directory
        (when gem-default-dir
          (let ((default-bin (expand-file-name "bin" gem-default-dir)))
            (when (file-directory-p default-bin)
              (add-to-list 'exec-path default-bin)
              (setenv "PATH" (concat default-bin ":" (getenv "PATH")))))))))

  ;; Manual command to install ruby-lsp if needed
  (defun mvh/install-ruby-lsp ()
    "Manually install ruby-lsp for the current Ruby version."
    (interactive)
    (message "Installing ruby-lsp...")
    (let* ((ruby-path (executable-find "ruby"))
           (use-user-install (string-prefix-p "/usr/bin" (or ruby-path "")))
           (install-cmd (if use-user-install
                           "gem install --user-install ruby-lsp"
                         "gem install ruby-lsp")))
      (if (zerop (call-process-shell-command install-cmd nil nil nil))
          (progn
            (mvh/update-gem-path-for-chruby)
            (message "ruby-lsp installed successfully"))
        (message "Failed to install ruby-lsp. Please install manually: %s" install-cmd))))

  ;; Configure ruby-lsp for bundler projects (simplified, no auto-install)
  (defun mvh/configure-ruby-lsp-for-project ()
    "Configure ruby-lsp to use bundler when appropriate."
    (when (fboundp 'lsp-mode)
      ;; Check if we're in a bundler project
      (let ((gemfile (locate-dominating-file default-directory "Gemfile")))
        (when gemfile
          ;; Check if ruby-lsp is in the Gemfile
          (let ((gemfile-path (expand-file-name "Gemfile" gemfile))
                (has-ruby-lsp nil))
            (when (file-exists-p gemfile-path)
              (with-temp-buffer
                (insert-file-contents gemfile-path)
                (setq has-ruby-lsp
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "ruby-lsp" nil t)))))
            ;; Set the buffer-local variable
            (setq-local lsp-ruby-lsp-use-bundler has-ruby-lsp))))))

  ;; Hook to configure ruby-lsp when entering ruby modes
  ;; Add with lower priority (positive depth) to run after chruby
  (add-hook 'ruby-mode-hook 'mvh/configure-ruby-lsp-for-project 90)
  (add-hook 'enh-ruby-mode-hook 'mvh/configure-ruby-lsp-for-project 90)

  ;; Configure lsp-mode settings for ruby-lsp (only after lsp-mode is loaded)
  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings
     '(("ruby-lsp.bundleGemfile" nil t)
       ("ruby-lsp.formatter" "auto" t))))

  ;; Create a wrapper script for ruby-lsp that handles Ruby version detection
  (defun mvh/create-ruby-lsp-wrapper ()
    "Create a wrapper script for ruby-lsp that works with chruby."
    (let ((wrapper-path (expand-file-name "~/.emacs.d/bin/ruby-lsp-wrapper")))
      (make-directory (file-name-directory wrapper-path) t)
      (with-temp-file wrapper-path
        (insert "#!/usr/bin/env bash\n"
                "# Wrapper script for ruby-lsp to work with chruby\n"
                "\n"
                "# Source chruby if available\n"
                "if [ -f /opt/homebrew/share/chruby/chruby.sh ]; then\n"
                "  source /opt/homebrew/share/chruby/chruby.sh\n"
                "elif [ -f /usr/local/share/chruby/chruby.sh ]; then\n"
                "  source /usr/local/share/chruby/chruby.sh\n"
                "fi\n"
                "\n"
                "# Auto-switch Ruby version based on .ruby-version\n"
                "if [ -f /opt/homebrew/share/chruby/auto.sh ]; then\n"
                "  source /opt/homebrew/share/chruby/auto.sh\n"
                "elif [ -f /usr/local/share/chruby/auto.sh ]; then\n"
                "  source /usr/local/share/chruby/auto.sh\n"
                "fi\n"
                "\n"
                "# Execute ruby-lsp\n"
                "exec ruby-lsp \"$@\"\n"))
      (set-file-modes wrapper-path #o755)
      wrapper-path))

  ;; Create the wrapper script at startup and configure LSP to use it
  (defvar mvh/ruby-lsp-wrapper-path nil
    "Path to the ruby-lsp wrapper script.")

  (setq mvh/ruby-lsp-wrapper-path (mvh/create-ruby-lsp-wrapper))
  ;; Add the wrapper directory to exec-path
  (add-to-list 'exec-path (file-name-directory mvh/ruby-lsp-wrapper-path))

  ;; Override the ruby-lsp server command to use our wrapper
  (with-eval-after-load 'lsp-mode
    (when mvh/ruby-lsp-wrapper-path
      (setq lsp-ruby-lsp-server-command (list mvh/ruby-lsp-wrapper-path))))

  (use-package lsp-ui
    :config (setq lsp-ui-sideline-mode nil
                    lsp-ui-flycheck-live-reporting nil
                    lsp-ui-sideline-enable nil
                    lsp-ui-sideline-show-diagnostics nil
                    ;; Enable useful doc features while keeping UI clean
                    lsp-ui-doc-enable t
                    lsp-ui-doc-show-with-cursor nil     ; Don't show on cursor hover
                    lsp-ui-doc-show-with-mouse t        ; Show on mouse hover
                    lsp-ui-doc-delay 0.5                ; Small delay for docs
                    lsp-ui-doc-max-height 20            ; Reasonable doc window size
                    lsp-ui-doc-max-width 80
                    ;; Peek window settings
                    lsp-ui-peek-always-show nil      ; Jump directly with single candidate
                    lsp-ui-peek-list-width 40
                    lsp-ui-peek-peek-height 15)
      :bind (:map
             lsp-mode-map
             ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)
             ([remap xref-find-references] . #'lsp-ui-peek-find-references)
             ;; Additional useful bindings
             ("C-c l D" . #'lsp-ui-doc-show)         ; Show docs on demand
             ("C-c l i" . #'lsp-ui-imenu))           ; LSP imenu
      :config
      ;; Add which-key descriptions for LSP UI commands
      (with-eval-after-load 'which-key
        (which-key-add-key-based-replacements
          "C-c l D" "show documentation"
          "C-c l i" "imenu (LSP)")))

(use-package consult-lsp
  :defer t
  :bind (:map lsp-mode-map
         ("C-c l s" . consult-lsp-symbols)     ; LSP symbols
         ("C-c l d" . consult-lsp-diagnostics) ; Project diagnostics
         ("C-c l f" . consult-lsp-file-symbols)) ; File symbols
  :config
  ;; Add xref integration for workspace symbol navigation
  (advice-add 'consult-lsp-symbols :before
              (lambda (&rest _) (xref-push-marker-stack)))

  ;; Add which-key descriptions for consult-lsp commands
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c l s" "workspace symbols"
      "C-c l d" "diagnostics"
      "C-c l f" "file symbols")))

;; Enhanced M-T binding with fallback to project-wide search
(defun mvh/workspace-symbols-or-search ()
  "Search workspace symbols via LSP if available, otherwise fall back to project grep."
  (interactive)
  (xref-push-marker-stack)
  (if (and (bound-and-true-p lsp-mode)
           (lsp-workspaces))
      (consult-lsp-symbols)
    (let ((term (thing-at-point 'symbol)))
      (consult-ripgrep (projectile-project-root) term))))

(global-set-key (kbd "M-T") 'mvh/workspace-symbols-or-search)

(advice-add 'isearch-forward :before (lambda (arg1 arg2) (xref-push-marker-stack)))
(advice-add 'isearch-backward :before (lambda (arg1 arg2) (xref-push-marker-stack)))

(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-debug-edit-template)
  :init
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  :config
  (require 'dap-gdb-lldb)

  (dap-register-debug-template
   "GDB::Run"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run"
         :target nil
         :cwd nil))

  (dap-register-debug-template
   "CRuby::Test"
   (list :type "gdb"
         :request "launch"
         :name "CRuby Test"
         :target "./miniruby"
         :cwd "${workspaceFolder}"
         :args (list "-I./lib" "-I." "-I.ext/common" "./tool/runruby.rb"
                     "--archdir=." "--extout=.ext" "test.rb")))

  (setq dap-ui-buffer-configurations
        '((dap-ui-locals-buffer . ((side . right) (slot . 1) (window-width . 0.30)))
          (dap-ui-expressions-buffer . ((side . right) (slot . 2) (window-width . 0.30)))
          (dap-ui-sessions-buffer . ((side . right) (slot . 3) (window-width . 0.30)))
          (dap-ui-breakpoints-buffer . ((side . left) (slot . 1) (window-width . 0.20)))
          (dap-ui-repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.25)))))

  (setq dap-tooltip-mode t)

  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))

  :bind
  (("C-c d d" . dap-debug)
   ("C-c d e" . dap-debug-edit-template)
   ("C-c d l" . dap-debug-last)
   ("C-c d r" . dap-debug-recent)
   :map dap-mode-map
   ("C-c d h" . dap-hydra)
   ("C-c d b" . dap-breakpoint-toggle)
   ("C-c d B" . dap-breakpoint-condition)
   ("C-c d n" . dap-next)
   ("C-c d s" . dap-step-in)
   ("C-c d o" . dap-step-out)
   ("C-c d c" . dap-continue)
   ("C-c d q" . dap-disconnect)
   ("C-c d Q" . dap-delete-all-sessions)
   ("C-c d i" . dap-ui-inspect-thing-at-point)
   ("C-c d w" . dap-ui-expressions-add)
   ("C-c d W" . dap-ui-expressions-remove))

  :config
  (dap-ui-mode 1)

  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c d" "debug"
      "C-c d d" "start debug"
      "C-c d e" "edit template"
      "C-c d l" "debug last"
      "C-c d r" "debug recent"
      "C-c d h" "hydra menu"
      "C-c d b" "toggle breakpoint"
      "C-c d B" "conditional breakpoint"
      "C-c d n" "next line"
      "C-c d s" "step in"
      "C-c d o" "step out"
      "C-c d c" "continue"
      "C-c d q" "disconnect"
      "C-c d Q" "quit all sessions"
      "C-c d i" "inspect"
      "C-c d w" "add watch"
      "C-c d W" "remove watch")))

;; Required dependencies
(use-package websocket)
(use-package transient)
(use-package web-server)  ; Required for MCP tools server

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :commands (claude-code-ide claude-code-ide-menu)
  :config
  ;; Enable MCP tools server
  (require 'claude-code-ide-emacs-tools)
  (setq claude-code-ide-enable-mcp-server t)
  (claude-code-ide-emacs-tools-setup)
  ;; Optional: Set window to right side at 50% width
  (setq claude-code-ide-use-side-window t
        claude-code-ide-window-position 'right
        claude-code-ide-window-width (/ (frame-width) 2))
  ;; Disable line numbers in Claude buffers
  (add-hook 'vterm-mode-hook
            (lambda ()
              (when (string-match-p "^\\*claude-code\\[" (buffer-name))
                (display-line-numbers-mode -1))))
  :bind
  (("C-c a a" . claude-code-ide)
   ("C-c a m" . claude-code-ide-menu)))

(defun ruby-frozen-string-literal ()
  "Check the current buffer for the magic comment # frozen_string_literal: true.
If the comment doesn't exist, offer to insert it."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (string= (thing-at-point 'line)
                     "# frozen_string_literal: true\n")
      (insert "# frozen_string_literal: true\n\n"))))

(use-package chruby
    :defer t  ; Load on demand, not immediately
    :init
    ;; Initialize chruby paths - this enables chruby integration
    (setq chruby-mode t)
    :config
    ;; Cache variables to avoid repeated shell commands
    (defvar mvh/chruby-cache (make-hash-table :test 'equal)
      "Cache for Ruby version gem paths to avoid repeated shell commands.")

    (defvar mvh/ruby-version-cache (make-hash-table :test 'equal)
      "Cache for .ruby-version file contents.")

    ;; Function to update PATH with gem bin directories - now with caching
    (defun mvh/update-gem-path-for-chruby ()
      "Update PATH to include gem bin directories for the current Ruby.
Uses caching to avoid repeated shell commands for the same Ruby version."
      (when-let ((ruby-path (executable-find "ruby")))
        ;; Check if we already have this Ruby's paths cached
        (let ((cached-paths (gethash ruby-path mvh/chruby-cache)))
          (if cached-paths
              ;; Use cached paths
              (progn
                (when (car cached-paths)
                  (add-to-list 'exec-path (car cached-paths))
                  (setenv "PATH" (concat (car cached-paths) ":" (getenv "PATH"))))
                (when (cdr cached-paths)
                  (add-to-list 'exec-path (cdr cached-paths))
                  (setenv "PATH" (concat (cdr cached-paths) ":" (getenv "PATH")))))
            ;; Not cached, fetch and cache (only happens once per Ruby version)
            (let* ((gem-info (shell-command-to-string "ruby -e 'puts Gem.user_dir; puts Gem.default_dir'"))
                   (gem-lines (split-string gem-info "\n" t))
                   (gem-user-dir (car gem-lines))
                   (gem-default-dir (cadr gem-lines))
                   user-bin default-bin)
              ;; Process user gem directory
              (when gem-user-dir
                (setq user-bin (expand-file-name "bin" gem-user-dir))
                (when (file-directory-p user-bin)
                  (add-to-list 'exec-path user-bin)
                  (setenv "PATH" (concat user-bin ":" (getenv "PATH")))))
              ;; Process default gem directory
              (when gem-default-dir
                (setq default-bin (expand-file-name "bin" gem-default-dir))
                (when (file-directory-p default-bin)
                  (add-to-list 'exec-path default-bin)
                  (setenv "PATH" (concat default-bin ":" (getenv "PATH")))))
              ;; Cache the paths for this Ruby
              (puthash ruby-path (cons user-bin default-bin) mvh/chruby-cache))))))

    ;; Auto-detect Ruby version from .ruby-version files with caching
    (defun mvh/chruby-use-corresponding ()
      "Use chruby to activate the Ruby version specified in .ruby-version.
Caches file contents to avoid repeated file reads."
      (when-let ((ruby-version-file (locate-dominating-file default-directory ".ruby-version")))
        (let* ((file-path (expand-file-name ".ruby-version" ruby-version-file))
               (file-mtime (nth 5 (file-attributes file-path)))
               (cached-data (gethash file-path mvh/ruby-version-cache))
               ruby-version)
          ;; Check cache validity
          (if (and cached-data
                   (equal (car cached-data) file-mtime))
              ;; Use cached version
              (setq ruby-version (cdr cached-data))
            ;; Read file and update cache
            (setq ruby-version (with-temp-buffer
                                (insert-file-contents file-path)
                                (string-trim (buffer-string))))
            (puthash file-path (cons file-mtime ruby-version) mvh/ruby-version-cache))
          ;; Only switch Ruby if needed
          (when (and ruby-version
                    (not (string-empty-p ruby-version))
                    (not (string= ruby-version (chruby-current))))
            (chruby ruby-version)
            (mvh/update-gem-path-for-chruby)))))

    ;; Simplified setup - no debug messages for performance
    (defun mvh/ruby-mode-chruby-setup ()
      "Setup chruby for Ruby mode efficiently."
      (mvh/chruby-use-corresponding))

    ;; Add with high priority (negative depth) to run early
    (add-hook 'ruby-mode-hook 'mvh/ruby-mode-chruby-setup -90)
    (add-hook 'enh-ruby-mode-hook 'mvh/ruby-mode-chruby-setup -90)
    (add-hook 'projectile-after-switch-project-hook 'mvh/chruby-use-corresponding)

    ;; Initialize chruby with default Ruby if available
    (when (file-exists-p (expand-file-name ".ruby-version" "~"))
      (let ((default-ruby (with-temp-buffer
                            (insert-file-contents (expand-file-name ".ruby-version" "~"))
                            (string-trim (buffer-string)))))
        (when (and default-ruby (not (string-empty-p default-ruby)))
          (chruby default-ruby)
          (mvh/update-gem-path-for-chruby)))))

  (use-package minitest :ensure t)

;; Use enh-ruby-mode for superior Ruby editing experience
(use-package enh-ruby-mode
  :mode ("\\(\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
         . enh-ruby-mode)
  :hook (enh-ruby-mode . subword-mode)
  :config
  ;; Disable encoding magic comments
  (setq enh-ruby-add-encoding-comment-on-save nil)

  ;; Enable the built-in indent bouncing - press TAB repeatedly to toggle
  (setq enh-ruby-bounce-deep-indent t)

  ;; Set hanging brace indent level
  (setq enh-ruby-hanging-brace-indent-level 2)

  ;; Start with shallow indenting by default
  (setq enh-ruby-deep-indent-construct nil)

  ;; Enable case-insensitive search in Ruby buffers
  (add-hook 'enh-ruby-mode-hook (lambda () (setq case-fold-search t)))

  ;; Configure indentation
  (setq enh-ruby-indent-level 2)
  (setq enh-ruby-indent-tabs-mode nil))

(let ((ruby-misc-dir "~/git/ruby/misc"))
  (if (file-directory-p ruby-misc-dir)
      (progn
        (add-to-list 'load-path ruby-misc-dir)
        (require 'ruby-style))))

(defun mvh/prog-mode-hook ()
  (define-key c-mode-map (kbd "C-c C-c") 'comment-or-uncomment-region))
(add-hook 'c-initialization-hook 'mvh/prog-mode-hook)

(defun cruby/compile-command ()
    "Returns a String representing the compile command to run for the given context"
    "make miniruby")

  (defun cruby/test-command ()
    "Returns a String representing the test command to run for the given context"
    (cond
     ((eq major-mode 'c-mode) "make btest")
     ((eq major-mode 'ruby-ts-mode)
      (format "make test-all TESTS=\"%s\"" (buffer-file-name)))
     ))

  ;; Declare function to avoid warnings
  (declare-function projectile-register-project-type "projectile")

  ;; Register project type after projectile is loaded
  (with-eval-after-load 'projectile
    (projectile-register-project-type 'cruby '("ruby.c" "kernel.rb" "yjit.c" )
                                      :compile 'cruby/compile-command
                                      :test 'cruby/test-command))

(use-package meson-mode)

(use-package rustic
  :bind (:map rustic-mode-map
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config (setq lsp-enable-symbol-highlighting nil
                lsp-signature-auto-activate nil
                rustic-format-on-save nil)
          (add-hook 'rustic-mode-hook 'mvh/rustic-mode-hook)

          ;; Add which-key descriptions for Rust commands
          (with-eval-after-load 'which-key
            (which-key-add-major-mode-key-based-replacements 'rustic-mode
              "C-c C-c" "rust commands"
              "C-c C-c a" "code action"
              "C-c C-c r" "rename symbol"
              "C-c C-c s" "analyzer status")))

(defun mvh/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but
  ;; don't try to save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved
  ;; this should no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package ess
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
      (list "http://thomasf.github.io/solarized-css/solarized-light.min.css"))

(use-package toml-mode
  :mode "\\.toml")

(use-package yaml-mode
  :mode "\\.yml"
        "\\.yaml")

(use-package markdown-mode
  :mode "\\.md"
        "\\.markdown")

(use-package org-make-toc
  :hook org-mode)
(use-package org
  :config (setq org-startup-truncated 1
                org-log-done 1)
          (add-to-list 'org-modules 'org-tempo t)

          (add-hook 'org-shiftup-final-hook 'windmove-up)
          (add-hook 'org-shiftleft-final-hook 'windmove-left)
          (add-hook 'org-shiftdown-final-hook 'windmove-down)
          (add-hook 'org-shiftright-final-hook 'windmove-right)

          (org-babel-do-load-languages 'org-babel-load-languages '((ruby . t)
                                                                   (emacs-lisp . t)
                                                                   (C . t)))
  :mode ("\\.org" . org-mode))

(use-package denote
  :custom (denote-directory "~/git/notes/misc")
  :bind ("C-x C-n" . denote))

(use-package org-journal
  :defer t
  :init (setq org-journal-prefix-key "C-c j ")
  :custom (org-journal-dir "~/git/notes/log_book/")
          (org-journal-file-format "%Y/%m/%d")
          (org-journal-date-format "%A %d %b %Y")
          (org-agenda-files "~/Documents/org/"))

(use-package org-static-blog
  :defer t
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

;; Customize the HTML output
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "~/org/"
         :base-extension "org"
         :publishing-directory "~/org/html"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers t
         :with-toc t
         :html-preamble t)

        ("images"
         :base-directory "~/org/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/org/html/images/"
         :publishing-function org-publish-attachment)

        ("other"
         :base-directory "~/org/other/"
         :base-extension "css\\|el"
         :publishing-directory "~/org/html/other/"
         :publishing-function org-publish-attachment)
        ("eightbitraptor" :components ("orgfiles" "images" "other"))))

(use-package mpc
  :defer t
  :init
  ;; Declare functions to avoid warnings
  (declare-function mpc-select-toggle "mpc")
  (declare-function mpc-playlist-add "mpc")
  (declare-function mpc-tagbrowser-all-p "mpc")
  (defun ebr/mpc-unselect-all (&optional event)
    "Unselect all selected songs in the current mpc buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((get-char-property (point) 'mpc-select)
          (let ((ols nil))
            (dolist (ol mpc-select)
              (if (and (<= (overlay-start ol) (point))
                       (> (overlay-end ol) (point)))
                  (delete-overlay ol)
                (push ol ols)))
            (cl-assert (= (1+ (length ols)) (length mpc-select)))
            (setq mpc-select ols)))
         ((mpc-tagbrowser-all-p) nil)
         (t nil))
        (forward-line 1))))
  (defun ebr/mpc-add-selected ()
    "Append to playlist, then unmark the song."
    (interactive)
    (mpc-playlist-add)
    (ebr/mpc-unselect-all))
  (defun ebr/mpc-add-at-point-and-unmark ()
    "Mark, append to playlist, then unmark the song."
    (interactive)
    (mpc-select-toggle)
    (mpc-playlist-add)
    (ebr/mpc-unselect-all))
  :custom
  (mpc-host "senjougahara")
  (mpc-songs-format "%2{Disc--}%3{Track} %28{Title} %18{Album} %18{Artist}")
  (mpc-browser-tags '(Artist Album))
  (mpc-cover-image-re "[Ff]older.jpg")
  :bind (:map mpc-mode-map
              ("a" . ebr/mpc-add-at-point-and-unmark)
              ("A" . ebr/mpc-add-selected)
              ("c" . ebr/mpc-unselect-all)
              ("d" . mpc-playlist-delete)
              ("p" . mpc-toggle-play)
              ("P" . mpc-playlist)
              ("s" . mpc-select)
              ("S" . mpc-stop))
  :config
  ;; Add which-key descriptions for MPC commands
  (with-eval-after-load 'which-key
    (which-key-add-major-mode-key-based-replacements 'mpc-mode
      "a" "add & unmark"
      "A" "add selected"
      "c" "clear selection"
      "d" "delete from playlist"
      "p" "play/pause"
      "P" "show playlist"
      "s" "select item"
      "S" "stop playback")))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))
