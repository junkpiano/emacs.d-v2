(setq package-enable-at-startup nil)

;; Backup and auto-save settings
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Startup screen inhibition
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))

(setq initial-scratch-message nil)

;; Font configuration
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
      '(("Noto Sans Mono CJK JP" . 1.00)   ;; Fine-tune around 1.00–1.05
        ("Source Han Code JP"    . 1.05)
        ("PlemolJP.*"            . 1.00)))
