;;; dired-hist-tl.el --- Traverse Dired buffer's history: back, forward -*- lexical-binding: t -*-
;; Copyright (C) 2024 github.com/Anoncheg1,codeberg.org/Anoncheg

;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Version: 0.14
;; Keywords: convenience, dired, history
;; URL: https://codeberg.org/Anoncheg/dired-hist

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
;; Not tested with all all tab-line options.
;; Package-Requires Emacs "27.1".

;; Commands:

;; `tab-line-switch-to-prev-tab' : Go back in Dired history
;; `tab-line-switch-to-next-tab' : Go forward in Dired history
;; `dired-hist-tl-dired-find-file' : Enter directory and clear forward

;; Features:

;; - History showed with tabs in every Dired window
;; - Tabs are sorted in order of creation, just as history
;; - Working under root console (theme modus-vivendi)
;; - Support only `dired-kill-when-opening-new-dired-buffer' is nil.
;; - If Dired was used in window we mark this window as Dired oriented.

;; Configuration:

;; (require 'dired-hist-tl)
;; (add-hook 'dired-mode-hook #'dired-hist-tl-dired-mode-hook)
;; (define-key dired-mode-map (kbd "RET") #'dired-hist-tl-dired-find-file)
;; (define-key dired-mode-map (kbd "^") #'dired-hist-tl-dired-up-directory)
;; (global-set-key (kbd "C-M-a") #'dired-hist-tl-tab-line-switch-to-prev-tab)
;; (global-set-key (kbd "C-M-e") #'dired-hist-tl-tab-line-switch-to-next-tab)

;; Testest with:
;; (global-tab-line-mode t)
;; (setopt tab-line-tabs-function #'tab-line-tabs-mode-buffers)
;; (setopt tab-line-switch-cycling t)

;; Customization:

;; There are no customization options at this time.

;; How it works:

;; We replaced `tab-line-tabs-function' function that return list of
;; tabs in `tab-line' mode.

;; By dafault Emacs create buffer for every new entered directory
;; (`dired-kill-when-opening-new-dired-buffer' is nil).  We use
;; modified functions to switch between opened buffers with
;; `tab-line-switch-to-prev-tab' and `tab-line-switch-to-next-tab'.

;; After we switch to prev-tab we need to close buffers at right, we
;; add this function as `dired-hist-tl-dired-find-file'.  Another issue
;; is that buffer list is not in history order, we create copy of
;; buffer list as variable `dired-hist-tl-buffer-list-ordered' and
;; synchrinize with original list but preserve order.

;; The problem with `tab-line-switch-to-prev-tab' that it uses
;; `switch-to-buffer' changing `window-prev-buffers' order of current buffer.
;; (switch-to-prev-buffer nil).

;; History is preserved as a list of buffers, used for both: history
;; and tabs displaying.
;; We display buffers as tabs: Dired buffers + other buffers.

;; For compatibility with `global-tab-line-mode' we save global
;; funcion that generate list of tabs and mark window in which Dired
;; mode was started.  In our `dired-hist-tl-tabs-mode-buffers-safe'
;; that generate list of tabs we check a mark of window and use saved
;; function if window don't have a mark.

;; Our replace functions to switch tabs didn't cause problems with
;; `global-tab-line-mode'.

;;; Code:
(require 'tab-line)
(require 'dired) ; for `dired-hist-tl-dired-find-file' only

(defvar dired-hist-tl--history nil
  "`buffer-list' with preserved order.")

(defun dired-hist-tl--go-up (history buffer-up buffer-current)
  "Move buffer BUFFER in place of BUFFER-CURRENT in HISTORY list.
Argument BUFFER-UP buffer that will be moved in front of BUFFER-CURENT."
  (let ((h (seq-copy history))
        (p)) ; to protect argument
        (setq h (delq buffer-up h))
        (setq p (1+ (seq-position h buffer-current)))
        (append (seq-take h p) (list buffer-up) (seq-drop h p))))

(defvar dired-hist-tl--saved-tab-line-tabs-function nil)
;; -- tests `dired-hist-tl--go-up'
;; (setq a '(1 2 3 4))
;; (if (not (equal (dired-hist-tl--go-up a 2 3) '(1 3 2 4)))
;;     (error "Test failed for dired-hist-tl-sync-two-lists"))
;; (if (not (equal (dired-hist-tl--go-up a 2 4) '(1 3 4 2)))
;;     (error "Test failed for dired-hist-tl-sync-two-lists"))
;; (if (not (equal (dired-hist-tl--go-up a 4 2) '(1 2 4 3)))
;;     (error "Test failed for dired-hist-tl-sync-two-lists"))


(defun dired-hist-tl--sort-main-according-to-second (main-list reference-list)
  "Sort only elemets of MAIN-LIST that exist in REFERENCE-LIST.
Return MAIN-LIST with sorted elements that exist in REFERENCE-LIST."
  (sort
   (copy-sequence main-list)
   (lambda (a b) (if (and (seq-contains-p reference-list a) (seq-contains-p reference-list b))
                     (< (seq-position reference-list a) (seq-position reference-list b))
                   ;; else
                   (< (seq-position main-list a) (seq-position main-list b))))))
;; -- test for
;; (setq main-list '(3 1 7 4 2 9))
;; (setq reference-list '(1 2 4))
;; (if (not (equal (dired-hist-tl--sort-main-according-to-second main-list reference-list)
;;                 '(3 1 7 2 4 9)))
;;     (error "Test failed for dired-hist-tl--sort-main-according-to-second"))

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

(defun dired-hist-tl--kill-right-tabs ()
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
               (tabs-right
                (seq-filter (lambda (b) (with-current-buffer b
                                          (derived-mode-p 'dired-mode)))
                            (nthcdr (1+ pos)  tabs)))
               (buffer))
          (if pos
              (dolist (tab tabs-right)
                (setq buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
                (if (and (bufferp buffer) ; skip buffers that is shown in windows
                         (not (get-buffer-window buffer t)))
                    (dired-hist-tl--close buffer tab)))))
        (force-mode-line-update)))

(defun dired-hist-tl--buffer-list-update-hook ()
  "Sync buffer-list-ordered with current `buffer-list'.
Executed at any interactions with tabs.
Keep only ordered list of Dired tabs."
  (setq dired-hist-tl--history
        (dired-hist-tl--sort-main-according-to-second
         (seq-filter (lambda (b) (and (buffer-live-p b)
                               (/= (aref (buffer-name b) 0) ?\s)))
              (seq-uniq (buffer-list))) ; Get `buffer-list' with alive filter.
         dired-hist-tl--history)))

(defun dired-hist-tl-tabs-buffer-list ()
  "Replacement of `tab-line-tabs-buffer-list' function.
To use `dired-hist-tl--history' replacement for `buffer-list'."
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (/= (aref (buffer-name b) 0) ?\s)))
              dired-hist-tl--history))

(defun dired-hist-tl--tabs-mode-buffers ()
  "Function to get a list of tabs to display in the tab line.
Return a list of buffers with  the same major mode as the current
buffer."
  (let ((mode major-mode))
    (seq-filter (lambda (b) (with-current-buffer b
                              (derived-mode-p mode)))
                (dired-hist-tl-tabs-buffer-list))))

(defun dired-hist-tl-tabs-mode-buffers-safe (&optional forswitch)
  "Wrapper for `dired-hist-tl--tabs-mode-buffers'.
That is safe for `global-tab-line-mode'.
Optional argument FORSWITCH used to flag that we get tab list for tab selection, because we show only Dired tabs for compact view."
  (if (not (window-parameter (selected-window) 'dired))
      ;; use global tab-line setting to be compatible with `global-tab-line-mode'
      (funcall dired-hist-tl--saved-tab-line-tabs-function)
    ;; else - this window was marked as Dired oriented.
    (if (or (not (derived-mode-p 'dired-mode)) forswitch)
        ;; Dired + others
        (let ((orig-sorted (dired-hist-tl--sort-main-according-to-second
                            (dired-hist-tl-tabs-buffer-list)
                            dired-hist-tl--history)))
          (append (seq-filter (lambda (b) (with-current-buffer b
                                            (derived-mode-p 'dired-mode))) orig-sorted) ; dird
                  (seq-filter (lambda (b) (with-current-buffer b
                                            (null (derived-mode-p 'dired-mode)))) orig-sorted))) ; not dird
      ;; else - get only Dired buffers
      (dired-hist-tl--tabs-mode-buffers))))

;; ------------------------------------------------------------------

;;;###autoload
(defun dired-hist-tl-dired-mode-hook ()
  "Activation hook, that should be added to `dired-mode-hook'.
Conflict with `global-tab-line-mode'."
  ;; required and safe - it is here just to keep configuration simplier
  (add-hook 'buffer-list-update-hook #'dired-hist-tl--buffer-list-update-hook)

  (make-local-variable 'tab-line-close-tab-function)
  ;; required to properly close tags at right when enter new directory
  (setq tab-line-close-tab-function 'kill-buffer)
  ;; save and set
  (if (null dired-hist-tl--saved-tab-line-tabs-function)
      (setq dired-hist-tl--saved-tab-line-tabs-function tab-line-tabs-function))
  ;; used to create tabs list
  ;; safe version of `dired-hist-tl--tabs-mode-buffers'
  (setq tab-line-tabs-function #'dired-hist-tl-tabs-mode-buffers-safe) ;
  ;; (setq tab-line-tabs-buffer-list-function #'dired-hist-tl-tabs-buffer-list)
  ;; activate tab-line-mode
  (tab-line-mode)
  ;; mark current window as Dired oriented - for compatibility with global-tab-line-mode
  (set-window-parameter (selected-window) 'dired t))

;;;###autoload
(defun dired-hist-tl-dired-find-file()
  "Close tabs to the right to clear forward history.
When when"
  (interactive)
  (dired-hist-tl--kill-right-tabs)
  (dired-find-file)
  ;; move current buffer to the front of history
  (if (not (eq (current-buffer) (car (last dired-hist-tl--history))))
    (setq dired-hist-tl--history
          (dired-hist-tl--go-up dired-hist-tl--history
                               (current-buffer)
                               (car (last dired-hist-tl--history)))))
  (dired-hist-tl-dired-mode-hook)) ; activation for new windows

;;;###autoload
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
  (dired-up-directory other-window)
  (dired-hist-tl-dired-mode-hook)) ; activation for new windows


(defun dired-hist-tl--switch-tab-next(prev &optional event)
  "If PREV is t switch to previous tab, if nil switch to next.
Tab list for switching from `tab-line-tabs-function' function.
Optional argument EVENT used four mouse interface."
  (let ((window (and (listp event) (posn-window (event-start event)))))
    (with-selected-window (or window (selected-window))
      (let* ((tabs (seq-filter
                    (lambda (tab) (or (bufferp tab) (assq 'buffer tab)))
                    (if (eq tab-line-tabs-function #'dired-hist-tl-tabs-mode-buffers-safe)
                        (dired-hist-tl-tabs-mode-buffers-safe t)
                        ;; else
                      (funcall tab-line-tabs-function))))
             (pos (seq-position
                   tabs (current-buffer)
                   (lambda (tab buffer)
                     (if (bufferp tab)
                         (eq buffer tab)
                       (eq buffer (cdr (assq 'buffer tab))))))) ; current buffer
             (tab (if pos ; calc next/prev tab
                      (if prev
                          (if (and tab-line-switch-cycling (<= pos 0))
                            (nth (1- (length tabs)) tabs)
                          (nth (1- pos) tabs))
                        ;; else - next
                        (if (and tab-line-switch-cycling (<= (length tabs) (1+ pos)))
                            (car tabs)
                          (nth (1+ pos) tabs)))))
             (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
        (when (bufferp buffer)
          (switch-to-buffer buffer))))))

;;;###autoload
(defun dired-hist-tl-tab-line-switch-to-prev-tab (&optional event)
  "Replacement for `tab-line-switch-to-prev-tab'.
Optional argument EVENT argument of original function."
  (interactive (list last-nonmenu-event))
  (dired-hist-tl--switch-tab-next t event))

;;;###autoload
(defun dired-hist-tl-tab-line-switch-to-next-tab (&optional event)
  "Replacement for `tab-line-switch-to-next-tab'.
Optional argument EVENT argument of original function."
  (interactive (list last-nonmenu-event))
  (dired-hist-tl--switch-tab-next nil event))

(provide 'dired-hist-tl)
;;; dired-hist-tl.el ends here
