![Eask badge](https://github.com/Anoncheg1/dired-hist/actions/workflows/test.yml/badge.svg?event=release)
![melpazoid badge](https://github.com/Anoncheg1/dired-hist/actions/workflows/melpazoid.yml/badge.svg)
[![Melpa](https://melpa.org/packages/dired-hist-badge.svg)](https://melpa.org/#/dired-hist)
[![Melpa Stable](https://stable.melpa.org/packages/dired-hist-badge.svg)](https://stable.melpa.org/#/dired-hist)

# dired-hist

Adds two commands: "back" and "forward", as buttons in every modern file managers: Thunar, Microsoft Explorer, Nautilus, Konqueror.

Emacs Minor mode for Dired built-in file manager, that traverse buffer history (or just path) according to history.

Original author is Karthik Chikmagalur https://github.com/karthink/dired-hist

Tested with Emacs 29.2.

## Features
- supported for two Dired modes: open folder in new buffer or in the same (dired-kill-when-opening-new-dired-buffer)
- if buffer is closed we remove his record from history
- history is global only for now

## Configuration

``` elisp
(require 'dired-hist)
(add-hook 'dired-mode-hook #'dired-hist-mode)
(define-key dired-mode-map (kbd "l") #'dired-hist-go-back)
(define-key dired-mode-map (kbd "r") #'dired-hist-go-forward)
(define-key dired-mode-map (kbd "C-c '") #'dired-hist-debug-activate)

```
Note: **l** and **r** keys are the defaults for **info-mode**, **help-mode** and **eww**, as well as for external packages like **pdf-tools**.

Consider instead "C-M-a" and "C-M-e".

## TODO
- simplify code, but raise required Emacs version. (26.1 for now)
- New feature: Allow to show history with help of tab-line-mode for ```dired-kill-when-opening-new-dired-buffer``` with t

# Alternative implementation based on tab-line-mode
Pros:
- simplier
- history is visible in tabs
- all buffers reused

Cons:
- support for only ```dired-kill-when-opening-new-dired-buffer``` is nil (default of Dired).
- bound to Tab Line mode and harder to customize.

## Featurs for alternative implementation
- Tabs are sorted in order of creation just as history
- When go up in directory tree you go back in history (if you don't like it then make me know)

## Configuration for alternative implementation

``` elisp
(require 'dired-hist-tl)
(add-hook 'dired-mode-hook #'dired-hist-tl-dired-mode-hook)
(define-key dired-mode-map (kbd "RET") #'dired-hist-tl-dired-find-file)
(define-key dired-mode-map (kbd "l") #'tab-line-switch-to-prev-tab)
(define-key dired-mode-map (kbd "r") #'tab-line-switch-to-next-tab)
```

# Other packages with own history implementation
- diredc - diredc-history-mode - Midnight Commander features (plus) for dired
- dirvish - extensions/dirvish-history.el - A modern file manager based on dired mode
