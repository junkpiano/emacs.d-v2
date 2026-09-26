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

;; Currency Symbols block (U+20A0-U+20BF).  Ricty Diminished has no glyph for
;; U+20BF BITCOIN SIGN (used by btc-ticker's mode-line format), so it renders
;; as tofu without this.  Noto Sans Mono is the only installed font covering
;; U+20BF; it lives in ~/.local/share/fonts (note: "Noto Sans Symbols 2" does
;; NOT cover this block, and Noto Sans Mono is a different family from the
;; "Noto Sans Mono CJK JP" used above).
(set-fontset-font t '(#x20a0 . #x20bf)
                  (font-spec :family "Noto Sans Mono")
                  nil 'append)

(setq face-font-rescale-alist
      '(("Noto Sans Mono CJK JP" . 1.00)   ;; Fine-tune around 1.00–1.05
        ("Source Han Code JP"    . 1.05)
        ("PlemolJP.*"            . 1.00)))
