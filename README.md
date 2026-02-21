
> **Note:** This Emacs setup is intended for Ubuntu 22.04.

# .emacs.d Configuration Repository

This repository contains personal Emacs configuration files and related package management setup.

## Required Fonts

To ensure proper display of Japanese and multilingual text, please install the following fonts:

- Ricty Diminished (ASCII)
- Noto Sans Mono CJK JP

These fonts are recommended for best compatibility and appearance with this configuration.

```
sudo apt update
# CJK + extras + emoji
sudo apt install -y fonts-noto-cjk fonts-noto-cjk-extra fonts-noto-color-emoji
# Ricty Diminished (if your repo has it). If not, install manually from its release page.
sudo apt install -y fonts-ricty-diminished || true
# Verify installed families
fc-list | grep -Ei "ricty|noto.*cjk|emoji"
```

## Features

### Package Management

- straight.el - Package manager
- use-package - Declarative package configuration

### Packages

- btc-ticker - Bitcoin price in mode line (JPY, 60s interval)
- mozc - Japanese input with popup candidates (TTY) and cursor colors
- org-mode - Babel eval enabled, done timestamps, gnuplot support
- company - Global autocomplete (120ms delay, 1 char prefix)
- rustic + lsp-mode - Rust dev with rust-analyzer, cargo fmt on save

### Custom Keybindings

- C-c " g (org-mode) - Plot gnuplot
- TAB (company active) - Complete selection
- C-c C-f (rustic-mode) - cargo fmt (package)
- C-c C-F (rustic-mode) - cargo fmt (workspace)



