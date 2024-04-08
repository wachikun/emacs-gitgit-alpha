;;; gitgit-status.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

;; Copyright (C) 2024 Tadashi Watanabe <twacc2020@gmail.com>

;; Author: Tadashi Watanabe <twacc2020@gmail.com>
;; Maintainer: Tadashi Watanabe <twacc2020@gmail.com>
;; Version: 
;; Keywords: tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; https://github.com/wachikun/emacs-gitgit-alpha

;;; Code:

(defface gitgit-status--face-git-mark '((((type x w32 mac ns)
                                          (class color)
                                          (background light))
                                         (:foreground "darkblue" :bold t
                                                      :underline t))
                                        (((type x w32 mac ns)
                                          (class color)
                                          (background dark))
                                         (:foreground "blue" :bold t
                                                      :underline t)))
  "mark"
  :group 'gitgit-faces)

(defface gitgit-status--face-git-staged-mark '((((type x w32 mac ns)
                                                 (class color)
                                                 (background light))
                                                (:foreground "darkblue" :bold t
                                                             :underline t
                                                             :background "gray60"))
                                               (((type x w32 mac ns)
                                                 (class color)
                                                 (background dark))
                                                (:foreground "blue" :bold t
                                                             :underline t
                                                             :background "gray60")))
  "git staged mark"
  :group 'gitgit-faces)

(defface gitgit-status--face-git-status-files '((((type x w32 mac ns)
                                                  (class color)
                                                  (background light))
                                                 (:foreground "orangered4" :bold t))
                                                (((type x w32 mac ns)
                                                  (class color)
                                                  (background dark))
                                                 (:foreground "orangered2" :bold t)))
  "git files"
  :group 'gitgit-faces)

(defface gitgit-status--face-git-staged-files '((((type x w32 mac ns)
                                                  (class color)
                                                  (background light))
                                                 (:foreground "orangered4" :bold t
                                                              :background "gray60"))
                                                (((type x w32 mac ns)
                                                  (class color)
                                                  (background dark))
                                                 (:foreground "orangered2" :bold t
                                                              :background "gray60")))
  "git staged files"
  :group 'gitgit-faces)

(defface gitgit-status--face-files '((((type x w32 mac ns)
                                       (class color)
                                       (background light))
                                      (:foreground "gray20" :bold t))
                                     (((type x w32 mac ns)
                                       (class color)
                                       (background dark))
                                      (:foreground "gray30" :bold t)))
  "files"
  :group 'gitgit-faces)

(defconst gitgit-status--mode-name "gitgit Status")
(defconst gitgit-status--file-name-regexp
  " \\(.+?\\)\\(?: -> .+\\)?$")

(define-derived-mode gitgit-status-mode
  nil
  gitgit-status--mode-name
  "Major mode for gitgit status"
  (define-key gitgit-status-mode-map "!" 'texe-process-mode-switch-to-texe)
  (define-key gitgit-status-mode-map "+" 'gitgit-status--mkdir)
  (define-key gitgit-status-mode-map "c" 'gitgit-status--run-commit)
  (define-key gitgit-status-mode-map "e" 'gitgit-status--run-find-file)
  (define-key gitgit-status-mode-map "f" 'gitgit-status--run-find-file)
  (define-key gitgit-status-mode-map "\C-m"
              'gitgit-status--run-find-file)
  (define-key gitgit-status-mode-map "m" 'gitgit-status--mark)
  (define-key gitgit-status-mode-map "u" 'gitgit-status--unmark)
  (define-key gitgit-status-mode-map "p" 'gitgit-status--previous-line)
  (define-key gitgit-status-mode-map "n" 'gitgit-status--next-line)
  (define-key gitgit-status-mode-map "g" 'gitgit-status--reload-process)
  (define-key gitgit-status-mode-map "\C-c\C-c" 'gitgit-status--reload-process)
  (define-key gitgit-status-mode-map "a" 'gitgit-status--git-add)
  (define-key gitgit-status-mode-map "b" 'gitgit-status--git-blame)
  (define-key gitgit-status-mode-map "d" 'gitgit-status--git-remove)
  (define-key gitgit-status-mode-map "l" 'gitgit-status--git-log)
  (define-key gitgit-status-mode-map "r" 'gitgit-status--git-auto-revert)
  (define-key gitgit-status-mode-map "R" 'gitgit-status--git-rename)
  (define-key gitgit-status-mode-map "Q" 'gitgit-status--quit)
  (define-key gitgit-status-mode-map "=" 'gitgit-status--git-diff-current-file)
  (define-key gitgit-status-mode-map (kbd "C-=") 'gitgit-status--git-diff-mark-files)
  (define-key gitgit-status-mode-map "*!" 'gitgit-status--unmark-all)
  (define-key gitgit-status-mode-map "*?" 'gitgit-status--mark-unknown)
  (define-key gitgit-status-mode-map "*A" 'gitgit-status--mark-add)
  (define-key gitgit-status-mode-map "*D" 'gitgit-status--mark-remove)
  (define-key gitgit-status-mode-map "*M" 'gitgit-status--mark-modified)
  (define-key gitgit-status-mode-map "*R" 'gitgit-status--mark-path-regexp)
  (define-key gitgit-status-mode-map "\M-."
              'gitgit-status--next-status-buffer)
  (define-key gitgit-status-mode-map "\M-,"
              'gitgit-status--previous-status-buffer)
  (define-key gitgit-status-mode-map (kbd "C-i") 'gitgit-status--next-line)
  (define-key gitgit-status-mode-map (kbd "C-S-i") 'gitgit-status--previous-line)
  ;; for texe-mode
  (define-key texe-mode-map "\C-c\C-v" 'gitgit-status--view-status-from-texe-buffer)
  (define-key texe-mode-process-mode-map "\C-c\C-v"
              'gitgit-status--view-status-from-texe-buffer)
  (set (make-local-variable 'gitgit-status-local-modified-files-function)
       nil)
  (set (make-local-variable 'gitgit-status-local-ignore-reload)
       nil)
  (set (make-local-variable 'gitgit-status-local-end-of-git-status-point)
       (point-min))
  (set (make-local-variable 'gitgit-status-local-end-of-files-point)
       (point-min))
  (set (make-local-variable 'gitgit-status-local-after-save-hash)
       (make-hash-table :test 'equal))
  (set (make-local-variable 'gitgit-status-local-recent-files-hash)
       (make-hash-table :test 'equal))
  (set (make-local-variable 'gitgit-status-local-mark-hash)
       (make-hash-table :test 'equal)))

(defun gitgit-status-update-faces ()
  (save-excursion
    (goto-char (point-min))
    (while (<= (point) gitgit-status-local-end-of-files-point)
      (texe-set-line-face-if-need)
      (gitgit-status--set-file-line-face)
      (forward-line 1))
    (while (< (point) (point-max))
      (texe-set-line-face-if-need)
      (forward-line 1))))

(defun gitgit-status-mark-hash (hash)
  (save-excursion
    (maphash #'(lambda (key _value)
                 (goto-char (point-min))
                 (when (gitgit-status--mark-re-search-forward key)
                   (gitgit-status--mark)))
             hash)))

;; (defun gitgit-status-set-mode-name-suffix (suffix)
;;   (setq mode-name (concat gitgit-status--mode-name " " suffix)))

(defun gitgit-status-sentinel-callback-reload-status-from-texe ()
  "texe 用の reload sentinel callback
texe 実行後は status が変化する可能性があるので、本 callback を呼び出して status を更新する。"
  (when (get-buffer (gitgit-status-get-status-buffer-name (buffer-name)))
    (with-current-buffer (gitgit-status-get-status-buffer-name (buffer-name))
      (gitgit-status--reload-status)
      (texe-update-point)
      (texe-set-header-line-process-success))))

(defun gitgit-status-get-status-buffer-name (buffer-name)
  (concat (replace-regexp-in-string "\\(.+\\*\\) .+"
                                    "\\1" buffer-name)
          " status"))

(defun gitgit-status-get-process-back-buffer-name (buffer-name)
  (texe-get-process-back-buffer-name (gitgit-status-get-status-buffer-name buffer-name)))

(defun gitgit-status--sentinel-callback-reload-status ()
  "gitgit-status 用の reload sentinel callback
commit/add/rm/restore など、実行後に status が変化する場合に呼び出す。"
  (texe-update-point)
  (texe-update-window-start texe-process-local-backup-point-alist)
  (texe-special-update-point texe-process-local-special-result)
  (texe-special-change-major-mode-if-match texe-process-local-special-result)
  (setq gitgit-status-local-ignore-reload t)
  (gitgit-status--reload-status))

(defun gitgit-status--sentinel-callback-reload-status-kill-process-buffer ()
  "gitgit-status 用の reload sentinel callback
commit/add/rm/restore など、実行後に status が変化する場合に呼び出す。"
  (setq texe-process-local-buffer-kill-p t)
  (gitgit-status--sentinel-callback-reload-status))

(defun gitgit-status--sentinel-callback-not-reload-status ()
  "gitgit-status 用の reload しない sentinel callback
diff/log など、実行後に status が変化しない場合に呼び出す。"
  (texe-update-point)
  (texe-update-window-start texe-process-local-backup-point-alist)
  (texe-special-update-point texe-process-local-special-result)
  (texe-special-change-major-mode-if-match texe-process-local-special-result))

(defun gitgit-status--get-command (search-special not-found)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" search-special)
                           nil
                           t)
        (let ((command (texe-get-current-line-command)))
          (if command command not-found))
      not-found)))

(defun gitgit-status--run-1 (no-display-process-buffer-p git-command buffer-name
                                                         sentinel-callback file-list command-filter
                                                         buffer-erase-p)
  (with-current-buffer (gitgit-get-texe-buffer-name-from-related-buffer)
    (let* (special (search-special (concat "#@gitgit-status-" git-command))
                   (command (gitgit-status--get-command search-special
                                                        gitgit-internal-git-command))
                   (args-alist (list (cons 'file-list file-list)
                                     (cons 'no-display-process-buffer no-display-process-buffer-p))))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^" search-special)
                                 nil
                                 t)
          (setq special (texe-get-line))))
      (when (string= command gitgit-internal-git-command)
        (setq command (concat command " " git-command)))
      (when file-list
        (setq command (concat command
                              " "
                              (mapconcat (lambda (s)
                                           (shell-quote-argument s))
                                         file-list
                                         " "))))
      (when command-filter
        (setq command (funcall command-filter command)))
      (texe-run-start-process nil special command
                              buffer-name args-alist sentinel-callback buffer-erase-p
                              nil t))))

(defun gitgit-status--run (no-display-process-buffer-p git-command buffer-name-suffix
                                                       sentinel-callback &optional file-list command-filter)
  (let ((buffer-name (concat (gitgit-get-texe-buffer-name-from-related-buffer)
                             " "
                             buffer-name-suffix)))
    (gitgit-status--run-1 no-display-process-buffer-p
                          git-command buffer-name sentinel-callback
                          file-list command-filter nil)))

(provide 'gitgit-status)
