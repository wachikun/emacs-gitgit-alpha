;;; gitgit-texe-updater.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defun gitgit-create-new-texes ()
  (interactive)
  (catch 'return
    (when (gitgit--texe-updater-verify)
      (throw 'return nil))
    (when (gitgit--texe-updater-create-new-if-needed)
      (throw 'return nil))))

(defun gitgit--texe-updater-verify ()
  (catch 'return
    (gitgit--texe-updater-mapc (lambda (file-name)
                                 (with-temp-buffer
                                   (insert-file-contents file-name)
                                   (goto-char (point-min))
                                   (unless (search-forward gitgit-texe-special-system-scripts)
                                     (message "Illegal texe: %s" file-name)
                                     (throw 'return t)))))
    nil))

(defun gitgit--texe-updater-create-new-if-needed ()
  (gitgit--texe-updater-mapc (lambda (file-name)
                               (with-temp-buffer
                                 (let ((original-buffer (current-buffer)))
                                   (insert-file-contents file-name)
                                   (with-temp-buffer
                                     (insert-buffer original-buffer)
                                     (goto-char (point-min))
                                     (search-forward gitgit-texe-special-system-scripts)
                                     (beginning-of-line)
                                     (delete-region (point)
                                                    (point-max))
                                     (insert gitgit-mode-default-texe-system)
                                     (when (gitgit--texe-updater-diff-p file-name
                                                                        (current-buffer)
                                                                        original-buffer)
                                       (write-region (point-min)
                                                     (point-max)
                                                     (concat file-name ".new")
                                                     nil
                                                     0)))))))
  nil)

(defun gitgit--texe-updater-mapc (func)
  (mapc #'(lambda (file-name)
            (when (string-match (concat "^"
                                        (regexp-quote gitgit-texe-file-name-prefix)
                                        ".+\\*") file-name)
              (funcall func
                       (concat gitgit-default-texe-directory file-name))))
        (directory-files gitgit-default-texe-directory)))

(defun gitgit--same-buffer-string-p (buffer-a buffer-b)
  (let ((case-fold-search nil))
    (= 0 (compare-buffer-substrings buffer-a nil nil
                                    buffer-b nil nil))))

(defun gitgit--texe-updater-diff-p (file-name buffer-old buffer-new)
  (if (gitgit--same-buffer-string-p buffer-old buffer-new)
      nil
    (let ((file-old (make-temp-file "old"))
          (file-new (make-temp-file "new"))
          diff-hash)
      (with-current-buffer buffer-old
        (write-region (point-min)
                      (point-max)
                      file-old
                      nil
                      0))
      (with-current-buffer buffer-new
        (write-region (point-min)
                      (point-max)
                      file-new
                      nil
                      0))
      (setq diff-hash (secure-hash 'sha224
                                   (replace-regexp-in-string "^[0-9].+$"
                                                             ""
                                                             (shell-command-to-string (concat "diff " file-old " " file-new)))))
      (message "%s: diff-hash = %s" file-name diff-hash)
      (delete-file file-old)
      (delete-file file-new)
      (not (equal gitgit-texe-diff-hash diff-hash)))))

(provide 'gitgit-texe-updater)
