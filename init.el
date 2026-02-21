

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
  :custom
  (default-input-method "japanese-mozc")
  (mozc-leim-title "[あ]")
  ;; GUI: overlay is the most stable
  (mozc-candidate-style 'overlay))

(use-package popup)
(use-package mozc-popup
  :after mozc
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
;; Org-mode for asset tracking
;; ============================

;; org is built-in, just configure it
(require 'org)

;; Do not ask confirmation for babel evaluation
(setq org-confirm-babel-evaluate nil)

;; Log timestamp when TODO is completed
(setq org-log-done 'time)

;; Better handling of numeric values in tables
(setq org-table-number-fraction 3)

(use-package gnuplot :straight t)

(org-defkey org-mode-map (kbd "C-c \" g") 'org-plot/gnuplot)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

(setq org-confirm-babel-evaluate nil)
;; 1) Company: global enable + sane defaults
(use-package company
  :ensure t
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

;; 2) rustic + lsp-mode (rust-analyzer)
(use-package rustic
  :ensure t
  :custom
  (rustic-lsp-client 'lsp)                 ;; use lsp-mode for RA
  (rustic-format-trigger nil))             ;; avoid double-formatting; we run cargo fmt elsewhere

(use-package lsp-mode
  :ensure t
  :hook (rustic-mode . lsp-deferred)
  :custom
  ;; Let company use the standard CAPF provided by lsp-mode
  (lsp-completion-provider :none)          ;; disable lsp's own company glue; use CAPF
  (lsp-idle-delay 0.3)
  (lsp-eldoc-render-all t)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")  ;; or "check"
  (lsp-enable-on-type-formatting nil))     ;; no on-type fmt (avoid flicker)

;; Ensure company uses CAPF (LSP) in LSP buffers
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-capf))))

(use-package rustic
  :ensure t
  :init
  ;; Use lsp-mode as the LSP client
  (setq rustic-lsp-client 'lsp)
  ;; Do NOT let rustic auto-format itself (we'll run `cargo fmt`)
  (setq rustic-format-trigger nil))

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

(defun my/rustic-cargo-fmt-on-save ()
  "Format current Rust package on save using `cargo fmt`."
  (when (derived-mode-p 'rustic-mode 'rust-mode)
    (my/cargo-fmt nil)))

;; Enable: format on save (package only). Use (my/cargo-fmt t) for workspace.
(add-hook 'rustic-mode-hook
          (lambda () (add-hook 'after-save-hook #'my/rustic-cargo-fmt-on-save nil t)))

;; Handy keys
(with-eval-after-load 'rustic
  (define-key rustic-mode-map (kbd "C-c C-f") (lambda () (interactive) (my/cargo-fmt nil)))
  (define-key rustic-mode-map (kbd "C-c C-F") (lambda () (interactive) (my/cargo-fmt t))))

