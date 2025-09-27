(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))

(setq initial-scratch-message nil)

(defvar y/ascii-font "Ricty Diminished")
(defvar y/jp-font    "Noto Sans Mono CJK JP")

(set-face-attribute 'default nil :family y/ascii-font :height 120)

(dolist (sc '(kana han cjk-misc bopomofo hangul))
  (set-fontset-font t sc (font-spec :family y/jp-font)))

(dolist (name '("japanese-jisx0208" "japanese-jisx0212" "katakana-jisx0201"))
  (let ((cs (ignore-errors (charset-by-name name))))
    (when cs
      (set-fontset-font t cs (font-spec :family y/jp-font)))))

(set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)

(setq face-font-rescale-alist
      '(("Noto Sans Mono CJK JP" . 1.00)   ;; 1.00〜1.05あたりで微調整
        ("Source Han Code JP"    . 1.05)
        ("PlemolJP.*"            . 1.00)))

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
  :straight (mozc
	     :type git
	     :host github
	     :repo "google/mozc"
	     :files ("src/unix/emacs/mozc.el"))
  :custom
  (default-input-method "japanese-mozc")
  (mozc-candidate-style 'overlay))

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
