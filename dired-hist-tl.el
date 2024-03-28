;;; dired-hist-tl.el --- Traverse Dired buffer's history: back, forward -*- lexical-binding: t -*-
;; Copyright (C) 2024  Anoncheg1

;; Author: Anoncheg1
;; Version: 0.13
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

;; dired-hist-tab-line is alternative approach to create history, back
;; and forward "buttons" for Dired mode.  Working only for
;; `dired-kill-when-opening-new-dired-buffer' set to nil.  tab-line
;; allow to see history at top of the buffer, but creates buffer for
;; every new folder.
;; Package-Requires Emacs "27.1".

;; Commands:

;; `tab-line-switch-to-prev-tab' : Go back in Dired history
;; `tab-line-switch-to-next-tab' : Go forward in Dired history
;; `dired-hist-tl-dired-find-file' : Enter directory and clear forward

;; Features:

;; - History showed with tabs in every Dired window
;; - Tabs are sorted in order of creation, just as history
;; - going back and up carefully programmed
;; - working under root console (theme modus-vivendi)
;; - Compatible with `global-tab-line-mode'
;; - Don't affect showed (live) windows

;; Configuration:

;; (require 'dired-hist-tl)
;; (add-hook 'dired-mode-hook #'dired-hist-tl-dired-mode-hook)
;; (define-key dired-mode-map (kbd "RET") #'dired-hist-tl-dired-find-file)
;; (define-key dired-mode-map (kbd "^") #'dired-hist-tl-dired-up-directory)
;; (define-key dired-mode-map (kbd "l") #'tab-line-switch-to-prev-tab)
;; (define-key dired-mode-map (kbd "r") #'tab-line-switch-to-next-tab)
;;
;; For better compatibility with `global-tab-line-mode' add:
;; (advice-add 'tab-line-switch-to-prev-tab :override #'dired-hist-tl-tab-line-switch-to-prev-tab)
;; (advice-add 'tab-line-switch-to-next-tab :override #'dired-hist-tl-tab-line-switch-to-next-tab)

;; Consider instead "C-M-a" and "C-M-e".

;; Customization:

;; There are no customization options at this time.

;; How it works:

;; By dafault Emacs create buffer for every new entered directory
;; (`dired-kill-when-opening-new-dired-buffer' is nil).  You can
;; switch between opened buffers with functions
;; `tab-line-switch-to-prev-tab' and `tab-line-switch-to-next-tab'.
;; After we switch to prev-tab we need to close buffers at right, we
;; add this function as `dired-hist-tl-dired-find-file'.  Another issue
;; is that buffer list is not in history order, we create copy of
;; buffer list as variable `dired-hist-tl-buffer-list-ordered' and
;; synchrinize with original list but preserve order.

;; History is preserved as a list of buffers, used for both: history
;; and tabs displaying.

;;; Code:
(require 'tab-line)
(require 'dired) ; for `dired-hist-tl-dired-find-file' only

(defvar dired-hist-tl--history nil
  "Mirror of `buffer-list' variable with preserving order.")

(defun dired-hist-tl--go-up (history buffer-up buffer-current)
  "Move buffer BUFFER in place of BUFFER-CURRENT in HISTORY list.
Argument BUFFER-UP buffer that will be moved in front of BUFFER-CURENT."
  (let ((h (seq-copy history))
        (p)) ; to protect argument
        (setq h (delq buffer-up h))
        (setq p (1+ (seq-position h buffer-current)))
        (append (seq-take h p) (list buffer-up) (seq-drop h p))))
;; -- tests `dired-hist-tl--go-up'
;; (setq a '(1 2 3 4))
;; (if (not (equal (dired-hist-tl--go-up a 2 3) '(1 3 2 4)))
;;     (error "Test failed for dired-hist-tl-sync-two-lists"))
;; (if (not (equal (dired-hist-tl--go-up a 2 4) '(1 3 4 2)))
;;     (error "Test failed for dired-hist-tl-sync-two-lists"))
;; (if (not (equal (dired-hist-tl--go-up a 4 2) '(1 2 4 3)))
;;     (error "Test failed for dired-hist-tl-sync-two-lists"))


(defun dired-hist-tl--sync-two-lists (l1 lbase)
  "Return L1 elemets ordered as same elements of LBASE.
L1 (buffer-list) provides elements, LBASE (history) provides order.
Steps:
  1) remove elements of lbase that don't exist in l1
  1.1 find elemets of lbase that not member of l1
  1.2 remove 1.1 from lbase
  2) l1-se = find elements of l1 that don't exist in lbase
  3) add to lbase l1-se)"
  ;; 1)
  ;; 1.1
  (let ((lb (copy-sequence lbase)) ; copy along the cdrs (not deep)
        (lbase-se (seq-filter (lambda (x) (not (member x l1)))  lbase))
        (l1-se))
    (mapc (lambda (x) (delq x lb)) lbase-se) ; 1.2
    ;; 2)
    (setq l1-se (seq-filter (lambda (x) (not (member x lb))) l1))
    (append lb l1-se) ; return
    ))
;; -- tests for `dired-hist-tl--sync-two-lists'
;; (setq vv '(1 3 4 5))
;; ;; (dired-hist-tl--sync-two-lists '(10 1 4 3) vv)
;; (if (not (equal (dired-hist-tl--sync-two-lists '(1 2 4 3) vv) '(1 3 4 2)))
;;     (error "Test failed for dired-hist-tl--sync-two-lists"))


(defun dired-hist-tl--close (buffer tab)
  "Close TAB with BUFFER, same as `tab-line-close-tab'.
Not bound to mouse event, accept BUFFER and TAB which are the same.
Depending on `tab-line-close-tab-function' BUFFER or TAB are used."
  (cond
   ((eq tab-line-close-tab-function 'kill-buffer) ;; used
    (kill-buffer buffer))
   ((eq tab-line-close-tab-function 'bury-buffer)
    (if (eq buffer (current-buffer))
        (bury-buffer)
      (set-window-prev-buffers nil (assq-delete-all buffer (window-prev-buffers)))
      (set-window-next-buffers nil (delq buffer (window-next-buffers)))))
   ((functionp tab-line-close-tab-function)
    (funcall tab-line-close-tab-function tab))))

(defun dired-hist-tl-kill-right-tabs ()
  "Close tabs at the right side of current tab.
Used to clear forward history in Dired mode, after we go back to left
and entered new folder."
      (with-selected-window (selected-window)
        (let* (;; list of tabs
               (tabs (seq-filter
                      (lambda (tab) (or (bufferp tab) (assq 'buffer tab)))
                      (funcall tab-line-tabs-function)))
               ;; get current buffer position in list of tabs
               (pos (seq-position
                     tabs (current-buffer)
                     (lambda (tab buffer)
                       (if (bufferp tab)
                           (eq buffer tab)
                         (eq buffer (cdr (assq 'buffer tab)))))))
               (tabs-right (nthcdr (1+ pos)  tabs))
               (buffer))
          (if pos
              (dolist (tab tabs-right)
                (setq buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
                (if (and (bufferp buffer) ; skip buffers that is shown in windows
                         (not (get-buffer-window buffer t)))
                    (dired-hist-tl--close buffer tab)))))
        (force-mode-line-update)))

(defun dired-hist-tl-buffer-list-update-hook ()
  "Sync buffer-list-ordered with current `buffer-list'."
  (setq dired-hist-tl--history
        (dired-hist-tl--sync-two-lists ; call
         (seq-filter (lambda (b) (with-current-buffer b ; arg1
                                   (derived-mode-p 'dired-mode))) (buffer-list))
         dired-hist-tl--history)) ; arg2
  )

(defun dired-hist-tl-tabs-buffer-list ()
  "Replacement of `tab-line-tabs-buffer-list' function.
To use `dired-hist-tl--history' replacement for `buffer-list'."
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (/= (aref (buffer-name b) 0) ?\s)))
              dired-hist-tl--history))

(defun dired-hist-tl--tabs-mode-buffers ()
  "Function to get a list of tabs to display in the tab line.
Return a list of buffers with the same major mode as the current buffer."
  (let ((mode major-mode))
    (seq-filter (lambda (b) (with-current-buffer b
                              (derived-mode-p mode)))
                (funcall tab-line-tabs-buffer-list-function))))

(defun dired-hist-tl-tabs-mode-buffers-safe ()
  "Wrapper for `dired-hist-tl--tabs-mode-buffers'.
That is safe for `global-tab-line-mode'."
  ;; if previous command was switch tab
  (if (and (or (eq last-command #'tab-line-switch-to-prev-tab) (eq last-command #'tab-line-switch-to-next-tab))
           ;; and previous buffer was not dired
           (not (with-current-buffer (car-safe (car-safe (window-prev-buffers (window-normalize-window nil t))))
                 (derived-mode-p 'dired-mode))))
      ;; we display tabs using global function
      (funcall (default-value 'tab-line-tabs-function))
    ;; else
    (dired-hist-tl--tabs-mode-buffers)))

(defun dired-hist-tl-dired-mode-hook ()
  "Activation hook, that should be added to `dired-mode-hook'.
Conflict with `global-tab-line-mode'."
  ;; required and safe - it is here just to keep configuration simplier
  (add-hook 'buffer-list-update-hook #'dired-hist-tl-buffer-list-update-hook)

  (make-local-variable 'tab-line-close-tab-function)
  (make-local-variable 'tab-line-tabs-function)
  (make-local-variable 'tab-line-tabs-buffer-list-function)
  ;; required to properly close tags at right when enter new directory
  (setq tab-line-close-tab-function 'kill-buffer)
  ;; used to create tabs list
  ;; safe version of dired-hist-tl--tabs-mode-buffers
  (setq tab-line-tabs-function #'dired-hist-tl-tabs-mode-buffers-safe)
  (setq tab-line-tabs-buffer-list-function #'dired-hist-tl-tabs-buffer-list)
  (tab-line-mode))

(defun dired-hist-tl-dired-find-file()
  "Close tabs to the right to clear forward history.
When when"
  (interactive)
  (dired-hist-tl-kill-right-tabs)
  (dired-find-file)
  ;; move current buffer to the front of history
  (if (not (eq (current-buffer) (car (last dired-hist-tl--history))))
    (setq dired-hist-tl--history
          (dired-hist-tl--go-up dired-hist-tl--history
                               (current-buffer)
                               (car (last dired-hist-tl--history))))))

(defun dired-hist-tl-dired-up-directory (&optional other-window)
  "Fix case when upper folder exist in history.
By default when we go up and if buffer exist we go backward.
Instead of that we reuse buffer and move buffer to the top of
history or in case we are in the middle of history to the place
of current buffer.
Optional argument OTHER-WINDOW dired-up-directory original argument."
  (interactive "P")
  ;; steps
  ;; 1) check if up folder exist as buffer
  ;; 2) move up buffer in front of current buffer
  ;; 3) switch to up buffer
  (let* ((dir (dired-current-directory))
         (up-path (file-name-directory (directory-file-name dir)))
         (up-buffer (car-safe (dired-buffers-for-dir up-path))))
    ;; (print (list "debug" dir up-path up-buffer dired-hist-tl--history))
    (if (and (not (equal dir up-path)) up-buffer)
        (setq dired-hist-tl--history
              (dired-hist-tl--go-up dired-hist-tl--history up-buffer (current-buffer)))))
  (dired-up-directory other-window))

(defun dired-hist-tl-tab-line-switch-to-prev-tab (&optional event)
  "Replacement for `tab-line-switch-to-prev-tab'.
For better compatibility with `global-tab-line-mode'.
Optional argument EVENT argument of original function."
  (interactive (list last-nonmenu-event))
  (let ((window (and (listp event) (posn-window (event-start event)))))
    (switch-to-prev-buffer window))) ; window is nil

(defun dired-hist-tl-tab-line-switch-to-next-tab (&optional event)
  "Replacement for `tab-line-switch-to-next-tab'.
For better compatibility with `global-tab-line-mode'.
Optional argument EVENT argument of original function."
  (interactive (list last-nonmenu-event))
  (let ((window (and (listp event) (posn-window (event-start event)))))
    (switch-to-next-buffer window)))

(provide 'dired-hist-tl)
;;; dired-hist-tl.el ends here
