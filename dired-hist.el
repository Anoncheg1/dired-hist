;;; dired-hist.el --- Traverse Dired buffer's history: back, forward -*- lexical-binding: t -*-
;; Copyright (C) 2024  Anoncheg1
;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Anoncheg1
;;      Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Created: 2022
;; Version: 0.13
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, dired, history
;; URL: https://github.com/Anoncheg1/dired-hist

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; dired-hist is a minor mode for Emacs that keeps track of visited
;; Dired buffers or paths and lets you go back and forwards across
;; them.  This is similar to the facility provided in Info, EWW and
;; in modern filemanagers.

;; Commands:

;; `dired-hist-mode'       : Turn on Dired history tracking
;; `dired-hist-go-back'    : Go back in Dired history
;; `dired-hist-go-forward' : Go forward in Dired history

;; Features:

;; - supported for  two Dired modes: open  folder in new buffer  or in
;;   the same (dired-kill-when-opening-new-dired-buffer)
;; - if buffer is closed we remove his record from history
;; - history is global

;; Configuration:

;; (require 'dired-hist)
;; (add-hook 'dired-mode-hook #'dired-hist-mode)
;; (define-key dired-mode-map (kbd "l") #'dired-hist-go-back)
;; (define-key dired-mode-map (kbd "r") #'dired-hist-go-forward)

;; Consider instead "C-M-a" and "C-M-e".

;; Customization:

;; There are no customization options at this time.

;; There is alternative implementation based on tab-line-mode.
;; Pros: less code, history is visible in tabs
;; Cons:
;; - support for only ```dired-kill-when-opening-new-dired-buffer``` is nil.
;; - tab-line modified globally for now

;; Configuration for alternative implementation:

;; (require 'dired-hist-tl)
;; (add-hook 'dired-mode-hook #'dired-hist-tl-dired-mode-hook)
;; (define-key dired-mode-map (kbd "RET") #'dired-hist-tl-dired-find-file)
;; (global-set-key (kbd "l") #'tab-line-switch-to-prev-tab)
;; (global-set-key (kbd "r") #'tab-line-switch-to-next-tab)

;; How it works:

;; There are two stacks: with history and for Forward key.
;; When we navigate the history grows, when we press back button
;; we transfer last record from history to forward stack.
;; Forward button do reverse - transfer from forward stack back to
;; history.
;;           .-> |-| -- Back >
;;         -/    | |   \---     <--.
;; -- Open/      |-|       \----    \-- Forward
;;               | |            \---
;;               | |                \->
;;               |-|                    |-|
;;               | |                    | |
;;               |-|                    |-|
;;               | |                    | |
;;               +-+                    +-+
;;         For Back <           For Forward >

;; About two modes:
;; Dired have two modes `dired-kill-when-opening-new-dired-buffer':
;; 1) create new buffer when visit folder
;; 2) recreate buffer every time, to keep only one
;; In 1) we are moves in history is switching between buffers
;; In 2) we are sitching between directories in one buffer
;; When in 1) we go to a new directory we just add new item to
;; hist-stack.
;; When in 2) we go to a new directory we also clear forward-stack.
;;
;; Special cases for nil `dired-kill-when-opening-new-dired-buffer':
;; - If buffer is closed then removed from history.
;; - When we change buffer manually history stay the same.
;; - if we changed manually buffer and go back we don't add it to
;;   stack.
;; Special case for t `dired-kill-when-opening-new-dired-buffer':
;; If two buffer opened: another will be deleted with
;;   `dired-hist-go-back' command at the end of the history stack by
;;   `find-alternate-file'.

;;; Change Log:

;; 0.10
;; - original from https://github.com/karthink/dired-hist
;; 0.11
;; - added debuging
;; - fixed behaviour for `dired-kill-when-opening-new-dired-buffer' t.
;; - comments, linting and push to MELPA
;; - emacs "26.1" preserved.
;; 0.12
;; - additinal debugging
;; - better behavior for dired-kill-when-opening-new-dired-buffer nil
;;   mode: when change buffer manually and close buffer.

;;; Code:
(require 'dired)

;; ---- two stacks if form of ((marker . directory-path) ...)
;; where marker is #<marker at 2366 in dired-hist.el> from (point-marker)
(defvar dired-hist-stack nil
  "The stack of previously visited Dired buffers.")

(defvar dired-hist-forward-stack nil
  "Forward history of previously visited Dired buffers.")

;; ---- debugging

(defvar dired-hist-debug nil)

(defun dired-hist-debug-activate ()
  "Handy when binded to key to fast activation."
  (interactive)
  (if dired-hist-debug
      (setq dired-hist-debug nil)
    ;; else

    (when (eq (length (window-list)) 1)
      (let ((cw (selected-window))
            (w-messages (split-window-horizontally))
            (w-buffers (split-window-vertically)))
        (select-window w-messages)
        (switch-to-buffer "*Messages*")
        (call-interactively #'end-of-buffer)
        (select-window w-buffers)
        (buffer-menu)
        (select-window cw)))
    (setq dired-hist-debug t))
  (print (list "dired-hist-debug:" dired-hist-debug)))

(defun dired-hist-debug-print (&optional where)
  "Debuggin output to *Messages* buffer.
Optional argument WHERE is a string to output firstly."
  (if where
      (print (concat "print at " where)))
  (print "hist stack:")
  (mapc #'print dired-hist-stack)
  (print "forward stack:")
  (mapc #'print dired-hist-forward-stack)
  ;; force wrap of messages to bottom and update buffers list
  (let ((cb (current-buffer)))
    (switch-to-buffer "*Buffer List*")
    (revert-buffer)
    (switch-to-buffer cb)

    (switch-to-buffer "*Messages*")
    (call-interactively #'end-of-buffer)
    (switch-to-buffer cb)))

;; ----

(defun dired-hist--match (stack)
  "Check current directory is in last element of STACK.
To not allow doubles.
For `dired-hist-stack' and `dired-hist-forward-stack'."
  (equal (cdr-safe (car-safe stack)) default-directory))

(defun dired-hist--update (&optional dont-touch-forward)
  "Add/create the `dired-hist-stack', previously visited paths.
If `dired-kill-when-opening-new-dired-buffer' enable, clear
forward stack also.
Optional argument DONT-TOUCH-FORWARD used during go-forward
command to prevent removing of forward stack."
  (unless (dired-hist--match dired-hist-stack)
    (push (cons (point-marker) default-directory) dired-hist-stack)
    ;; clean forward stack if we are in dired 2) mode
    (if (and dired-kill-when-opening-new-dired-buffer (null dont-touch-forward))
        (setq dired-hist-forward-stack nil)))
  ;; debug
  (if dired-hist-debug (dired-hist-debug-print "Update")))

(defun dired-hist-go-back ()
  "Transfer element from hist-stack to forward-stack."
  (interactive)
  ;; Transfer element from hist-stack to forward-stack.
  ;; we should remove element from dired-hist-stack if doesn't nil
  ;; and have at least 2 elements.
  ;; (when (>= (proper-list-p dired-hist-stack) 2) ; depend on (emacs "27.1")
  (when (cdr-safe dired-hist-stack)
    ;; just remove if double found
    (if (dired-hist--match dired-hist-forward-stack)
        (pop dired-hist-stack)
      ;; else ; transfer
      (push (pop dired-hist-stack) dired-hist-forward-stack)))
  ;; 3) go to last element of hist-stack
  ;; if not already at it.
  (if (not (equal default-directory (cdr (car dired-hist-stack))))
      (dired-hist--visit (car dired-hist-stack)))
  ;; debug
  (if dired-hist-debug (dired-hist-debug-print "Go-Back")))

(defun dired-hist-go-forward ()
  "Transfer element from forward-stack to hist-stack."
  (interactive)
  (when dired-hist-forward-stack
    ;; remove element from forward-stack
    (remove-hook 'dired-mode-hook #'dired-hist--update)

    (dired-hist--visit (pop dired-hist-forward-stack))
    (add-hook 'dired-mode-hook #'dired-hist--update)
    ;; `dired-hist--update' but without removing
    ;; add current path to hist-stack
    ;; we don't transfer, to enshure that there is not duplicates
    (dired-hist--update t))
  ;; debug
  (if dired-hist-debug (dired-hist-debug-print "Go-Forward")))

(defun dired-hist--visit (item)
  "Visit Dired buffer or directory specified in ITEM.
ITEM is a cons cell in form of (marker . directory-path)."
  ;; (debug-on-variable-change 'dired-hist-forward-stack)
  (let* ((last-buffer (marker-buffer (car item)))
         (alive-p (buffer-live-p last-buffer))
         (win (and alive-p
                   (get-buffer-window last-buffer))))
    (cond
     (win (select-window win))
     (alive-p (switch-to-buffer last-buffer))
     (t  (if (and dired-kill-when-opening-new-dired-buffer
                  (eq major-mode 'dired-mode))
             ;; just change current folder
             ;; (dired-noselect (cdr item)) ; (expand-file-name (cdr item))
             ;; (dired--find-possibly-alternative-file (cdr item)) ; depend on (emacs "28.1")
             ;; (dired--find-file #'find-alternate-file (cdr item)) ; depend on (emacs "28.1")
             (find-alternate-file (cdr item)) ; may remove buffer
            ;; else create new buffer
            (dired (cdr item)))))))


(defun dired-hist-kill-buffer-hook ()
  "Remove first found current buffer from stacks in step:
1) Try to find current buffer in hist stack
2) try to find current buffer in forward stack
Additional just in case:
3) try to find path in hist stack
4) try to find path in forward stack"
  (if (and (eq major-mode 'dired-mode) (null dired-kill-when-opening-new-dired-buffer))
      (let ((cb (current-buffer))
            (new-stack)
            (item))
        ;; 1) clear hist stack
        (setq new-stack
              (seq-remove (lambda (x)
                            (eq cb (marker-buffer (car x))))
                            dired-hist-stack))
        ;; if length did not changed - clear forward stack
        (if (not (eq (length new-stack) (length dired-hist-stack)))
            ;; then apply new-stack
            (setq dired-hist-stack new-stack)
          ;; else 2)
          (setq new-stack
                (seq-remove (lambda (x)
                              (eq cb (marker-buffer (car x))))
                            dired-hist-forward-stack))
          ;; if length did not changed - remove path
          (if (not (eq (length new-stack) (length dired-hist-forward-stack)))
              ;; then apply new-stack
              (setq dired-hist-forward-stack new-stack)
            ;; else - 3) remove path from hist stack
            (setq item (rassoc default-directory dired-hist-stack))
            (if item
                (setq dired-hist-stack (remove item dired-hist-stack))
              ;; else - 4) remove path from hist stack
              (setq item (rassoc default-directory dired-hist-forward-stack))
              (if item
                  (setq dired-hist-stack (remove item dired-hist-forward-stack)) ;
              )))))))


;; require (emacs 29.1)
;; (defvar-keymap dired-hist-map
;;   "C-M-a" #'dired-hist-go-back
;;   "C-M-e" #'dired-hist-go-forward
;;   "C-c '" #'dired-hist-debug-activate)

;;;###autoload
(define-minor-mode dired-hist-mode
  "Provide history commands for visited directories.
Keep track of visited Dired buffers and switch between them."
  :group 'dired-hist
  :global t
  :lighter nil
  ;; :keymap dired-hist-map
  (if dired-hist-mode
      (progn
        ;; if mode added as hook to `dired-mode-hook' or activated
        ;; during Dired mode.
        (if (eq major-mode 'dired-mode)
            (dired-hist--update t))
        ;; if mode activated globally
        (add-hook 'dired-mode-hook #'dired-hist--update)
        (if (null dired-kill-when-opening-new-dired-buffer)
            (add-hook 'kill-buffer-hook #'dired-hist-kill-buffer-hook)))
    ;; else
    (remove-hook 'dired-mode-hook #'dired-hist--update)
    (setq dired-hist-stack nil
          dired-hist-forward-stack nil)))

(provide 'dired-hist)
;;; dired-hist.el ends here
