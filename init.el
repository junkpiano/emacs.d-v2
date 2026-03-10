(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Install from your local git repo via file:// URL
(use-package btc-ticker
  :straight (btc-ticker :type git
                        :host github
                        :repo "junkpiano/btc-ticker.el"
                        :files ("btc-ticker.el"))
  :custom
  (btc-ticker-currency "JPY")
  (btc-ticker-interval 60)
  :config
  (btc-ticker-mode 1))

(use-package mozc
  :straight (mozc :type git
                  :host github
                  :repo "google/mozc"
                  :files ("src/unix/emacs/*.el"))
  :custom
  (default-input-method "japanese-mozc")
  (mozc-leim-title "[あ]")
  ;; GUI: overlay is the most stable
  (mozc-candidate-style 'overlay))

(use-package popup
  :straight t)

(use-package mozc-popup
  :straight (mozc-popup :type git
                        :host github
                        :repo "iRi-E/mozc-el-extensions"
                        :files ("mozc-popup.el"))
  :after (mozc popup)
  :config
  ;; TTY only
  (unless (display-graphic-p)
    (require 'mozc-popup)
    (setq mozc-candidate-style 'popup)))


(use-package mozc-cursor-color
  :straight (mozc-cursor-color
             :type git
             :host github
             :repo "iRi-E/mozc-el-extensions"
            :files ("mozc-cursor-color.el"))
  :after mozc
  :custom
  (mozc-cursor-color-alist
   '((direct . "#BD93F9") (hiragana . "#CC3333") (read-only . "#84A0C6"))))

;; ============================
;; Org-mode configuration
;; ============================

;; org is built-in, just configure it
(require 'org)

;; Directory and file settings
(setq org-directory "~/org/")
(setq org-default-notes-file (expand-file-name "index.org" org-directory))
(setq org-agenda-files (list org-directory))

;; Appearance settings
(setq org-hide-emphasis-markers t)       ; Hide markup markers like * / _
(setq org-startup-folded 'content)       ; Start with headings folded
(setq org-startup-indented t)            ; Visual indentation
(setq org-pretty-entities t)             ; Display UTF-8 characters for entities
(setq org-hide-leading-stars t)          ; Clean up heading stars
(setq org-return-follows-link t)         ; RET follows links

;; TODO and logging
(setq org-log-done 'time)                ; Log timestamp when TODO is completed
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Better handling of numeric values in tables
(setq org-table-number-fraction 3)

;; Do not ask confirmation for babel evaluation
(setq org-confirm-babel-evaluate nil)

;; Enable syntax highlighting in src blocks
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Key bindings for org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Org capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline org-default-notes-file "Notes")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Gnuplot support for asset tracking
(use-package gnuplot :straight t)

(org-defkey org-mode-map (kbd "C-c \" g") 'org-plot/gnuplot)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)
   (emacs-lisp . t)
   (shell . t)
   (python . t)))

;; 1) Company: global enable + sane defaults
(use-package company
  :straight t
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.12)                ;; popup after ~120ms
  (company-minimum-prefix-length 1)        ;; start from 1 char
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-show-numbers t))

;; Optional: TAB to complete when candidates are visible
(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))

;; 2) Rust via built-in rust-ts-mode + eglot (rust-analyzer)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode) . ("rust-analyzer"))))

(add-hook 'rust-ts-mode-hook #'eglot-ensure)

;; Helper: find Cargo project root
(defun my/rust-project-root ()
  "Return Cargo project root or signal error."
  (or (locate-dominating-file default-directory "Cargo.toml")
      (user-error "No Cargo.toml found above %s" default-directory)))

(defun my/cargo-fmt (&optional workspace)
  "Run `cargo fmt` at project root.
If WORKSPACE is non-nil, run with `--all` (format the whole workspace)."
  (interactive "P")
  (let* ((root (my/rust-project-root))
         (default-directory root)
         (buf (get-buffer-create "*cargo fmt*"))
         (args (append '("fmt") (when workspace '("--all"))))
         (cur (current-buffer)))
    (save-buffer)
    (with-current-buffer buf (erase-buffer))
    (let ((exit (apply #'call-process "cargo" nil buf t args)))
      (if (zerop exit)
          (progn
            ;; reload current buffer in case rustfmt changed it on disk
            (when (buffer-file-name cur)
              (with-current-buffer cur
                (revert-buffer :ignore-auto :noconfirm)))
            (message "cargo fmt OK%s" (if workspace " (workspace)" " (package)")))
        (display-buffer buf)
        (error "cargo fmt failed with exit %s" exit)))))

(defun my/rust-cargo-fmt-on-save ()
  "Format current Rust package on save using `cargo fmt`."
  (when (derived-mode-p 'rust-mode 'rust-ts-mode)
    (my/cargo-fmt nil)))

;; Enable Rust defaults for both classic and tree-sitter modes.
(defun my/rust-mode-setup ()
  "Enable local Rust editing helpers."
  (add-hook 'after-save-hook #'my/rust-cargo-fmt-on-save nil t)
  (local-set-key (kbd "C-c C-f") (lambda () (interactive) (my/cargo-fmt nil)))
  (local-set-key (kbd "C-c C-F") (lambda () (interactive) (my/cargo-fmt t))))

(add-hook 'rust-ts-mode-hook #'my/rust-mode-setup)

;; Turn off the annoying beeping sound
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (progn
    (set-message-beep 'silent)))
 ((string-equal system-type "gnu/linux") ; Linux
  (progn
    (setq visible-bell t))))

;; Better bullet points (optional but recommended)
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; ============================
;; General enhancements
;; ============================

;; Pkl mode - Major mode for editing Pkl configuration files
(use-package pkl-mode
  :straight (pkl-mode :type git
                      :host github
                      :repo "sin-ack/pkl-mode"
                      :files ("pkl-mode.el"))
  :custom
  (pkl-enable-copilot nil))

;; Markdown mode
(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

;; TypeScript mode
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :custom
  (typescript-indent-level 2))

;; Which-key: Display available keybindings in popup
(use-package which-key
  :straight t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5))

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Text zooming (zoom in/out across all buffers)
(use-package default-text-scale
  :straight t
  :config
  (default-text-scale-mode 1))
;; Keybindings:
;; C-M-= or C-M-+ : Increase text size (zoom in)
;; C-M--          : Decrease text size (zoom out)
;; C-M-0          : Reset to default size

;; ============================
;; Auto-revert: Critical for Claude Code workflow
;; ============================
;; Automatically refresh buffers when files change on disk
;; This ensures open buffers update when Claude Code edits files
(global-auto-revert-mode 1)

;; Also auto-revert non-file buffers like dired
(setq global-auto-revert-non-file-buffers t)

;; Revert without asking, keep it quiet
(setq auto-revert-verbose nil)

;; ============================
;; Treemacs: Project explorer
;; ============================
(use-package treemacs
  :straight t
  :config
  ;; Performance + UX defaults that work well for monorepos
  (setq treemacs-width 34
        treemacs-follow-after-init t
        treemacs-is-never-other-window nil
        treemacs-recenter-after-file-follow nil
        treemacs-silent-refresh t
        treemacs-sorting 'alphabetic-asc

        ;; Git integration: 'simple for performance
        ;; On macOS, 'deferred can be heavy - start conservative
        treemacs-git-mode 'simple)

  ;; Auto-follow current file buffer
  (treemacs-follow-mode 1)

  ;; Filewatch mode: crucial for external edits (Claude Code)
  ;; On macOS, file notification can be finicky depending on Emacs build
  ;; If you notice lag, this is the knob to adjust
  (treemacs-filewatch-mode 1)

  ;; Optional: keep treemacs updated when you change workspaces/projects
  (treemacs-fringe-indicator-mode 'always))

;; Treemacs keybindings
(global-set-key (kbd "C-x t t") #'treemacs)
(global-set-key (kbd "C-x t f") #'treemacs-find-file)
(global-set-key (kbd "C-x t 1") #'treemacs-delete-other-windows)

;; Optional: Treemacs icons in dired (nice visual consistency)
(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; NOTE: If you add magit or projectile later, uncomment these integrations:
;; (use-package treemacs-projectile :straight t)
;; (use-package treemacs-magit :straight t)

;; ============================
;; Face customization for better readability
;; ============================
;; Change blue directory text to cyan for better contrast on dark backgrounds
;; Change purple syntax highlighting to more readable colors
(custom-set-faces
 '(dired-directory ((t (:foreground "cyan"))))
 '(treemacs-directory-face ((t (:foreground "cyan"))))
 ;; Strings: change from purple to light salmon
 '(font-lock-string-face ((t (:foreground "#FFA07A"))))
 ;; Documentation strings
 '(font-lock-doc-face ((t (:foreground "#98FB98"))))
 ;; Constants: change to light cyan
 '(font-lock-constant-face ((t (:foreground "#8BE9FD"))))
 ;; Built-in functions: change to light coral if purple
 '(font-lock-builtin-face ((t (:foreground "#DDA0DD")))))

;; ============================
;; MacOS specific settings
;; ============================
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  ;; Smooth scrolling on macOS
  (setq mac-mouse-wheel-smooth-scroll t))

(global-set-key (kbd "C-c SPC") 'set-mark-command)
