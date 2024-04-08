;;; gitgit-status-hook.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defun gitgit-status-after-save-hook-add-modified-files (check-file-name)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" gitgit-texe-special-comment-modified-files
                                     "$")
                             nil
                             t)
      (let* ((backup-buffer-read-only buffer-read-only)
             (dir-regexp (concat "^"
                                 (expand-file-name default-directory)))
             (cooked-check-file-name (replace-regexp-in-string dir-regexp "" check-file-name)))
        (unless (gethash cooked-check-file-name gitgit-status-local-after-save-hash)
          (puthash cooked-check-file-name t gitgit-status-local-after-save-hash)
          (forward-line 1)
          (beginning-of-line)
          (setq buffer-read-only nil)
          (insert (concat cooked-check-file-name "\n"))
          (forward-line -1)
          (gitgit-status--set-file-line-face)
          (setq buffer-read-only backup-buffer-read-only))))))

(defun gitgit-status-after-save-hook-rerun (_check-file-name)
  (gitgit-status--rerun-process))

(defun gitgit-status--after-save-hook ()
  (mapc #'(lambda (buffer)
            (when (string-match gitgit-texe-status-buffer-regexp (buffer-name buffer))
              (let ((check-buffer-file-name buffer-file-name))
                (unless (string-match "/\\.git/" check-buffer-file-name)
                  (with-current-buffer buffer
                    (let ((dir-regexp (concat "^"
                                              (expand-file-name default-directory))))
                      (when (string-match dir-regexp check-buffer-file-name)
                        (funcall gitgit-status-local-modified-files-function
                                 check-buffer-file-name))))))))
        (buffer-list)))

(defun gitgit-status--buffer-list-update-hook-1 (check-buffer-file-name buffer)
  (when (string-match gitgit-texe-status-buffer-regexp (buffer-name buffer))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^" gitgit-texe-special-comment-recent-files
                                         "$")
                                 nil
                                 t)
          (beginning-of-line)
          (let ((dir-regexp (concat "^"
                                    (expand-file-name default-directory))))
            (when (string-match dir-regexp check-buffer-file-name)
              (let ((backup-buffer-read-only buffer-read-only)
                    (cooked-buffer-file-name (replace-regexp-in-string dir-regexp "" check-buffer-file-name))
                    (start-point (point))
                    limit-point)
                (save-excursion
                  (re-search-forward "^$" nil t)
                  (setq limit-point (point)))
                (setq buffer-read-only nil)
                ;; delete cooked-buffer-file-name
                (save-excursion
                  (when (re-search-forward (concat "^" cooked-buffer-file-name "$")
                                           limit-point
                                           t)
                    (delete-region (line-beginning-position)
                                   (1+ (line-end-position)))))
                (remhash check-buffer-file-name gitgit-status-local-recent-files-hash)
                ;; insert cooked-buffer-file-name
                (forward-line 1)
                (insert (concat cooked-buffer-file-name "\n"))
                (forward-line -1)
                (gitgit-status--set-file-line-face)
                ;; 扱いやすいよう下記のような冗長な構造に
                ;;   key: check-buffer-file-name
                ;; value: (list check-buffer-file-name cooked-buffer-file-name sort-key)
                (puthash check-buffer-file-name
                         (list check-buffer-file-name
                               cooked-buffer-file-name
                               (current-time))
                         gitgit-status-local-recent-files-hash)
                ;; delete gitgit-recent-files-limit
                (goto-char start-point)
                (re-search-forward "^$" nil t)
                (when (> (1- (count-lines start-point
                                          (point))) gitgit-recent-files-limit)
                  (forward-line -1)
                  (remhash (nth 0
                                (nth 0
                                     (sort (hash-table-values gitgit-status-local-recent-files-hash)
                                           (lambda (a b)
                                             (time-less-p (nth 2 a)
                                                          (nth 2 b))))))
                           gitgit-status-local-recent-files-hash)
                  (delete-region (line-beginning-position)
                                 (1+ (line-end-position))))
                (setq buffer-read-only backup-buffer-read-only)))))))))

(defun gitgit-status--buffer-list-update-hook ()
  (when (and buffer-file-name
             (not (string-match "\\(/\\.git/\\)\\|\\(^ \\)"
                                buffer-file-name)))
    (let ((check-buffer-file-name buffer-file-name))
      (mapc #'(lambda (buffer)
                (gitgit-status--buffer-list-update-hook-1
                 check-buffer-file-name buffer))
            (buffer-list)))))

(add-hook 'after-save-hook 'gitgit-status--after-save-hook)
(add-hook 'buffer-list-update-hook 'gitgit-status--buffer-list-update-hook)

(provide 'gitgit-status-hook)
