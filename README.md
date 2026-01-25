
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

- Package management using [straight.el](https://github.com/radian-software/straight.el)
- Support for mozc (Japanese input method)

## About mozc Installation

To use mozc, you need to build it from source. This is necessary because the latest version of Emacs is not compatible with the mozc packages distributed by most package managers. Building mozc requires [Bazel](https://bazel.build/) to be installed.
It is recommended to install Bazel via [Bazelisk](https://github.com/bazelbuild/bazelisk). Please refer to the Bazelisk GitHub repository for installation instructions.

### Example: Building mozc

1. Install Bazel.
2. Obtain the mozc source code.
3. Build mozc using Bazel.
4. Configure Emacs to use the built mozc.

For detailed instructions, please refer to the [official mozc repository](https://github.com/google/mozc) or your distribution's documentation.

## About straight.el

This configuration uses straight.el for package management. It will be installed automatically on the first launch.

For more information, see the [straight.el official documentation](https://github.com/radian-software/straight.el).

## License

Each file and package follows its respective license.

