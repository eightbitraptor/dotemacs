# Emacs Literate Configuration Context

## Key Concepts

- **Literate Configuration**: Org-mode documents containing code blocks that tangle to produce Emacs configuration files
- **Tangling**: Process of extracting source code from org-mode blocks to generate .el files
- **Org-babel**: Org-mode's system for working with source code blocks
- **Native Compilation**: Emacs 28+ feature that compiles Elisp to native machine code for performance
- **Early-init.el**: Emacs 27+ file loaded before package system and GUI initialization
- **Lexical Binding**: Modern Elisp scoping mechanism for better performance and cleaner code
- **Byte Compilation**: Intermediate compilation step that converts .el to .elc bytecode

## Common Patterns

### Basic Literate Config Structure
```org
#+TITLE: Emacs Configuration
#+PROPERTY: header-args:emacs-lisp :tangle ./init.el :mkdirp yes
#+STARTUP: content

* Early Init
:PROPERTIES:
:header-args:emacs-lisp: :tangle ./early-init.el
:END:

#+begin_src emacs-lisp
;;; early-init.el --- Early Init -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(setq native-comp-async-report-warnings-errors nil)
#+end_src

* Init
#+begin_src emacs-lisp
;;; init.el --- Main configuration -*- lexical-binding: t; -*-
;; Main configuration starts here
#+end_src
```
**When to use**: Starting a new literate configuration
**Expected output**: Tangling produces early-init.el and init.el

### Performance Optimization Pattern
```emacs-lisp
;; Garbage collection optimization
(setq gc-cons-threshold (* 50 1000 1000)) ; 50MB during init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)))) ; 2MB after init

;; File handler optimization
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-speed 3)
  (setq native-comp-deferred-compilation t))
```
**When to use**: In early-init.el for startup optimization
**Expected output**: Faster startup time, deferred native compilation

### Conditional Tangling Pattern
```org
* Package Management
#+begin_src emacs-lisp :tangle (when (eq system-type 'gnu/linux) "./init.el")
;; Linux-specific configuration
(setq some-linux-var t)
#+end_src

#+begin_src emacs-lisp :tangle (when (eq system-type 'darwin) "./init.el")
;; macOS-specific configuration
(setq some-macos-var t)
#+end_src
```
**When to use**: Platform-specific configurations
**Expected output**: Code tangled only on matching systems

### Noweb Reference Pattern
```org
* Constants
#+name: paths
#+begin_src emacs-lisp :tangle no
(defconst my-cache-dir (expand-file-name "cache/" user-emacs-directory))
(defconst my-data-dir (expand-file-name "data/" user-emacs-directory))
#+end_src

* Core Setup
#+begin_src emacs-lisp :noweb yes
<<paths>>
(unless (file-directory-p my-cache-dir)
  (make-directory my-cache-dir t))
#+end_src
```
**When to use**: Reusing code blocks across sections
**Expected output**: Expanded code with noweb references resolved

## Implementation Details

### Tangle Configuration from Command Line
```bash
# Tangle all blocks
emacs --batch -l org --eval "(org-babel-tangle-file \"config.org\")"

# Tangle with specific target
emacs --batch -l org --eval "(org-babel-tangle-file \"config.org\" \"init.el\")"

# Async tangle script
#!/usr/bin/env bash
emacs --batch --eval "
(require 'org)
(setq org-confirm-babel-evaluate nil)
(org-babel-tangle-file \"$1\")
"
```
**Validation**: Check if init.el exists and has recent timestamp
**Common errors**: `Cannot open load file` = org not loaded

### Profile Startup Time
```emacs-lisp
;; Add to early-init.el
(defvar my-init-time nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq my-init-time (format "%.2f seconds"
                                      (float-time
                                       (time-subtract after-init-time before-init-time))))))

;; Profile specific sections
(defmacro measure-time (label &rest body)
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06f" ,label (float-time (time-since time)))))

;; Usage in config
(measure-time "Loading packages"
  (require 'package)
  (package-initialize))
```
**Validation**: Check *Messages* buffer for timing output
**Common errors**: Timing includes tangling time = run from tangled files

### Debugging Literate Config
```emacs-lisp
;; Debug tangling issues
(setq org-babel-use-quick-and-dirty-noweb-expansion t)
(setq org-src-preserve-indentation t)

;; Trace specific functions
(defun my-debug-tangle ()
  (interactive)
  (let ((org-babel-pre-tangle-hook
         (cons (lambda () (message "Tangling %s" (buffer-file-name)))
               org-babel-pre-tangle-hook)))
    (org-babel-tangle)))

;; Conditional debug blocks
#+begin_src emacs-lisp :tangle (when (getenv "EMACS_DEBUG") "./init.el")
(setq debug-on-error t)
(setq debug-on-quit t)
#+end_src
```
**Validation**: Run with `EMACS_DEBUG=1 emacs`
**Common errors**: Silent tangle failures = check *Org-Babel Error* buffer

### Byte Compilation Integration
```emacs-lisp
;; Auto-compile tangled files
(defun my-tangle-and-compile ()
  (interactive)
  (org-babel-tangle)
  (byte-recompile-directory user-emacs-directory 0))

;; Native compilation control
(defun my-native-compile-config ()
  (interactive)
  (native-compile-async user-emacs-directory 'recursively))

;; Check compilation status
(defun my-check-native-comp ()
  (interactive)
  (message "Native comp available: %s, files: %d"
           (featurep 'native-compile)
           (length (directory-files comp-eln-load-path nil "\\.eln$"))))
```
**Validation**: Check `comp-eln-load-path` for .eln files
**Common errors**: `comp-eln-load-path is nil` = native-comp not available

## Troubleshooting

### Tangling Produces Empty Files
**Symptoms**: init.el exists but is empty or has only header
**Cause**: Missing `:tangle yes` in header-args or syntax error in org
**Fix**: 
```org
#+PROPERTY: header-args:emacs-lisp :tangle yes :comments link
# Or per-block:
#+begin_src emacs-lisp :tangle yes
```

### Slow Startup Despite Optimizations
**Symptoms**: Emacs takes >2 seconds to start
**Cause**: Package loading, autoloads, or file handlers
**Fix**:
```emacs-lisp
;; Defer package loading
(setq package-enable-at-startup nil)
(setq package-quickstart t) ; Generate autoloads cache

;; Profile to find bottlenecks
(require 'profiler)
(profiler-start 'cpu)
(add-hook 'after-init-hook #'profiler-report)
```

### Native Compilation Errors
**Symptoms**: Warnings about "Error during redisplay"
**Cause**: Async native compilation warnings
**Fix**:
```emacs-lisp
(setq native-comp-async-report-warnings-errors nil)
(setq warning-suppress-types '((comp)))
```

### Cross-Platform Path Issues
**Symptoms**: Config works on Linux but fails on macOS
**Cause**: Hardcoded paths or system-specific commands
**Fix**:
```emacs-lisp
(defconst is-linux (eq system-type 'gnu/linux))
(defconst is-mac (eq system-type 'darwin))

(defun my-system-path (linux-path mac-path)
  (if is-linux linux-path mac-path))

;; Usage
(setq my-tool-path (my-system-path "/usr/bin/tool" "/opt/homebrew/bin/tool"))
```

### Org-babel Evaluation Blocked
**Symptoms**: "Evaluation of code-block is disabled"
**Cause**: Security feature blocking code execution
**Fix**:
```emacs-lisp
;; Disable confirmation for specific languages
(setq org-confirm-babel-evaluate nil) ; Disable all

;; Or selectively
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "emacs-lisp")))
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
```

## Authoritative References
- [GNU Emacs Manual - Init File](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html)
- [Org-mode Manual - Working with Source Code](https://orgmode.org/manual/Working-with-Source-Code.html)
- [Emacs Lisp Reference - Byte Compilation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Byte-Compilation.html)
- [Emacs Native Compilation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Native-Compilation.html)
- [Org-babel Documentation](https://orgmode.org/worg/org-contrib/babel/)
- [Emacs Wiki - Literate Configuration](https://www.emacswiki.org/emacs/LiterateConfiguration)