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
;; history

;; Features:

;; - tabs are sorted in order of creation just as history

;; Configuration:

;; (require 'dired-hist-tl)
;; (add-hook 'dired-mode-hook #'dired-hist-tl-dired-mode-hook)
;; (define-key dired-mode-map (kbd "RET") #'dired-hist-tl-dired-find-file)
;; (global-set-key (kbd "l") #'tab-line-switch-to-prev-tab)
;; (global-set-key (kbd "r") #'tab-line-switch-to-next-tab)

;; Consider instead "C-M-a" and "C-M-e".

;; Customization:

;; There are no customization options at this time.

;; How it works:

;; By dafault Emacs create buffer for every new entered directory
;; (`dired-kill-when-opening-new-dired-buffer' is nil).  You can
;; switch between opened buffers with functions
;; `tab-line-switch-to-prev-tab' and `tab-line-switch-to-next-tab'.
;; After we switch to prev-tab we need to close buffers at right, we
;; add this function as `dired-hist-tl-dired-find-file'. Another issue
;; is that buffer list is not in history order, we create copy of
;; buffer list as variable `dired-hist-tl-buffer-list-ordered' and
;; synchrinize with original list but preserve order.

;; History is preserved as a list of buffers, used for both: history
;; and tabs displaying.

;;; Code:
(require 'tab-line)
(require 'dired) ; for `dired-hist-tl-dired-find-file' only

(defvar dired-hist-tl-buffer-list-ordered (buffer-list)
  "Mirror of `buffer-list' variable with preserving order.")

(defun dired-hist-tl-close (buffer tab)
  "Close TAB with BUFFER, same as `tab-line-close-tab'.
Not bound to mouse event, accept BUFFER and TAB which as the same.
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
  "Kill tabs at the right side of current tab.
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
              ;; (print (list pos tabs-right))
              (dolist (tab tabs-right)
                (setq buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
                (if (bufferp buffer)
                    ;; (print buffer)
                    (dired-hist-tl-close buffer tab)))))
        (force-mode-line-update)))

(defun dired-hist-tl-sync-two-lists (l1 lbase)
  "Return L1 elemets ordered as same elements of LBASE.
L1 (buffer-list) provides elements, LBASE provides order.
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
;; tests for `dired-hist-tl-sync-two-lists'
;; (setq vv '(1 3 4 5))
;; (if (not (equal (dired-hist-tl-sync-two-lists '(1 2 4 3) vv) '(1 3 4 2)))
;;     (error "Test failed for dired-hist-tl-sync-two-lists"))

(defun dired-hist-tl-buffer-list-update-hook ()
  "Sync buffer-list-ordered with current `buffer-list'."
  (setq dired-hist-tl-buffer-list-ordered
        (dired-hist-tl-sync-two-lists (buffer-list)
                                      dired-hist-tl-buffer-list-ordered))
  ;; (print (mapcar 'buffer-name dired-hist-tl-buffer-list-ordered))
)

(defun dired-hist-tl-tabs-buffer-list ()
  "Replacement of `tab-line-tabs-buffer-list' function.
To use `dired-hist-tl-buffer-list-ordered' replacement for `buffer-list'."
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (/= (aref (buffer-name b) 0) ?\s)))
              dired-hist-tl-buffer-list-ordered))

(defun dired-hist-tl-tabs-mode-buffers ()
  "Function to get a list of tabs to display in the tab line.
Return a list of buffers with the same major mode as the current buffer."
  (let ((mode major-mode))
    (seq-filter (lambda (b) (with-current-buffer b
                              (derived-mode-p mode)))
                (funcall tab-line-tabs-buffer-list-function))))

(defun dired-hist-tl-dired-mode-hook ()
  "Should be added to `dired-mode-hook'."
  ;; required and safe - it is here just to keep configuration simplier
  (add-hook 'buffer-list-update-hook #'dired-hist-tl-buffer-list-update-hook)
  ;; required to properly close tags at right when enter new directory
  (make-local-variable 'tab-line-close-tab-function)
  (setq tab-line-close-tab-function 'kill-buffer)
  ;; used to create tabs list
  (make-local-variable 'tab-line-tabs-function)
  (setq tab-line-tabs-function #'dired-hist-tl-tabs-mode-buffers)
  (make-local-variable 'tab-line-tabs-buffer-list-function)
  (setq tab-line-tabs-buffer-list-function #'dired-hist-tl-tabs-buffer-list)
  (tab-line-mode))

(defun dired-hist-tl-dired-find-file()
  "Close tabs to the right to clear forward history."
  (interactive)
  (dired-hist-tl-kill-right-tabs)
  (dired-find-file))

(provide 'dired-hist-tl)
;;; dired-hist-tl.el ends here
