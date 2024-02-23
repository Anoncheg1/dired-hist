;;; dired-hist.el --- Traverse Dired buffer history: back, forward -*- lexical-binding: t -*-
;; Copyright (C) 2024  Anoncheg1
;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Anoncheg1
;;      Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Created: 2022
;; Version: 0.11
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience
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
;; Dired buffers and lets you go back and forwards across them.  This is
;; similar to the facility provided in other Emacs major modes, such as
;; Info and EWW.

;; Commands:

;; `dired-hist-mode'       : Turn on Dired history tracking
;; `dired-hist-go-back'    : Go back in Dired history
;; `dired-hist-go-forward' : Go forward in Dired history

;; Configuration:

;; (require 'dired-hist)
;; (add-hook 'dired-mode-hook #'dired-hist-mode)
;; (define-key dired-mode-map (kbd "C-M-a") #'dired-hist-go-back)
;; (define-key dired-mode-map (kbd "C-M-e") #'dired-hist-go-forward)

;; Customization:

;; There are no customization options at this time.

;; How it works:

;; Dired have two modes `dired-kill-when-opening-new-dired-buffer':
;; 1) create new buffer when visit folder
;; 2) recreate buffer every time, to keep only one
;; In 1) we are moves in history is switching between buffers
;; In 2) we are sitching between directories in one buffer
;; When in 1) we go to a new directory we just add new item to
;; hist-stack.
;; When in 2) we go to a new directory we also clear forward-stack.

;;; Change Log:

;; 0.10
;; - original from https://github.com/karthink/dired-hist
;; 0.11
;; - added debuging
;; - fixed behaviour for `dired-kill-when-opening-new-dired-buffer'
;;   two modes.
;; - comments, linting and push to MELPA

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
    (setq dired-hist-debug t)
    (when (eq (length (window-list)) 1)
      (let ((cw (selected-window))
            (window (split-window-horizontally)))
        (select-window window)
        (switch-to-buffer "*Messages*")
        (call-interactively #'end-of-buffer)
        (select-window cw))))
  (print (list "dired-hist-debug" dired-hist-debug)))

(defun dired-hist-debug-print ()
  "Debug."
    (print "hist stack:")
    (cl-loop for b in dired-hist-stack
             do (print b))
    (print "forward stack:")
    (cl-loop for b in dired-hist-forward-stack
             do (print b))
    ;; force wrap of messages to bottom
    (let ((cw (current-buffer)))
      (switch-to-buffer "*Messages*")
      (call-interactively #'end-of-buffer)
      (switch-to-buffer cw)))

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
Optional argument DONT-TOUCH-FORWARD used during go-forward command to prevent removing of forward stack."
  (unless (dired-hist--match dired-hist-stack)
    (push (cons (point-marker) default-directory) dired-hist-stack)
    ;; clean forward stack if we are in dired 2) mode
    (if (and dired-kill-when-opening-new-dired-buffer (null dont-touch-forward))
        (setq dired-hist-forward-stack nil)))
  ;; debug
  (if dired-hist-debug (dired-hist-debug-print)))

(defun dired-hist-go-back ()
  "Transfer element from hist-stack to forward-stack."
  (interactive)
  ;; 1) update
  (dired-hist--update)
  ;; 2) Transfer element from hist-stack to forward-stack.
  ;; we should remove element from dired-hist-stack if doesn't nil
  ;; and have at least 2 elements.
  ;; (when (>= (proper-list-p dired-hist-stack) 2) ; depend on (emacs "27.1")
  (when (cdr-safe dired-hist-stack)
    ;; just remove if double found
    (if (dired-hist--match dired-hist-forward-stack)
        (pop dired-hist-stack)
      ;; else ; transfer
      (push (pop dired-hist-stack) dired-hist-forward-stack))
    ;; 3) go to last element of hist-stack
    (dired-hist--visit (car dired-hist-stack))))

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
    (dired-hist--update t)))

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
             (find-alternate-file (cdr item))
            ;; else create new buffer
            (dired (cdr item)))))))

;; require (emacs 29.1)
;; (defvar-keymap dired-hist-map
;;   "C-M-a" #'dired-hist-go-back
;;   "C-M-e" #'dired-hist-go-forward
;;   "C-c '" #'dired-hist-debug-activate)

;;;###autoload
(define-minor-mode dired-hist-mode
  "Keep track of visited Dired buffers and switch between them."
  :group 'dired-hist
  :global t
  :lighter nil
  ;; :keymap dired-hist-map
  (if dired-hist-mode
      (progn
        (dired-hist--update t)
        (add-hook 'dired-mode-hook #'dired-hist--update))
    ;; else
    (remove-hook 'dired-mode-hook #'dired-hist--update)
    (setq dired-hist-stack nil
          dired-hist-forward-stack nil)))

(provide 'dired-hist)
;;; dired-hist.el ends here
