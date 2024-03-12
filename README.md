![Eask badge](https://github.com/Anoncheg1/dired-hist/actions/workflows/test.yml/badge.svg?event=release)
![melpazoid badge](https://github.com/Anoncheg1/dired-hist/actions/workflows/melpazoid.yml/badge.svg)

# dired-hist

Adds two commands: "back" and "forward", as buttons in every modern file managers: Thunar, Microsoft Explorer, Nautilus, Konqueror.

Emacs Minor mode for Dired built-in file manager, that traverse buffer history (or just path) according to history.

# Features
- supported for two Dired modes: open folder in new buffer or in the same (dired-kill-when-opening-new-dired-buffer)
- if buffer is closed we remove his record from history
- history is global only for now

# Configuration

``` elisp
(require 'dired-hist)
(define-key dired-mode-map (kbd "l") #'dired-hist-go-back)
(define-key dired-mode-map (kbd "r") #'dired-hist-go-forward)
(define-key dired-mode-map (kbd "C-c '") #'dired-hist-debug-activate)
(add-hook 'dired-mode-hook #'dired-hist-mode)
```
Note: **l** and **r** keys are the defaults for **info-mode**, **help-mode** and **eww**, as well as for external packages like **pdf-tools**.

Consider instead "C-M-a" and "C-M-e".

# TODO
- simplify code, but raise required Emacs version. (26.1 for now)
- New feature: Allow to show history with help of tab-line-mode for ```dired-kill-when-opening-new-dired-buffer``` with t


# Other packages with own history implementation
- diredc - diredc-history-mode - Midnight Commander features (plus) for dired
- dirvish - extensions/dirvish-history.el - A modern file manager based on dired mode
