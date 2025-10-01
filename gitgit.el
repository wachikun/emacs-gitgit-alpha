;;; gitgit.el -- Git User Interface -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(require 'texe)
(require 'gitgit-vars)
(require 'gitgit-setup)
(require 'gitgit-status)
(require 'gitgit-status-interactive)
(require 'gitgit-status-hook)
(require 'gitgit-blame)
(require 'gitgit-diff)
(require 'gitgit-grep)
(require 'gitgit-log)
(require 'gitgit-texe-updater)

(defun gitgit (initial-directory &optional rerun-p)
  (interactive "Ddirectory: ")
  (gitgit-setup initial-directory rerun-p))

(defun gitgit-version ()
  (interactive)
  (message gitgit-version-string))

(defun gitgit-get-texe-buffer-name-from-related-buffer ()
  "gitgit status/blame/diff/log などの関連バッファ名から texe-bufer-name を取得する
取得できるのはあくまでもバッファ名が関連バッファフォーマットである場合で、 texe-process などの
任意のバッファ名からは取得できないことに注意。
(texe-process では texe-process-local-args-alist の i-texe-buffer-name から取得する)"
  (replace-regexp-in-string " [^ ]+$"
                            ""
                            (buffer-name)))

(defun gitgit-switch-to-texe ()
  (interactive)
  (switch-to-buffer (gitgit-get-texe-buffer-name-from-related-buffer)))

(defun gitgit-switch-to-status ()
  (interactive)
  (switch-to-buffer (gitgit-status-get-status-buffer-name (gitgit-get-texe-buffer-name-from-related-buffer))))

(defun gitgit-toggle-diff-command ()
  (interactive)
  (if gitgit-status-diff-command
      (setq gitgit-status-diff-command nil)
    (setq gitgit-status-diff-command "difftool")))

(defun gitgit-update-buffer-header-line (branch)
  (setq header-line-format (concat (propertize default-directory 'face 'gitgit--face-git-default-directory)
                                   "  "
                                   (propertize branch
                                               'face
                                               (if (string= branch "master")
                                                   'gitgit--face-git-branch-master
                                                 'gitgit--face-git-branch))
                                   "  ")))

(defun gitgit-get-current-branch ()
  (let ((status-buffer-name (gitgit-status-get-status-buffer-name (gitgit-get-texe-buffer-name-from-related-buffer))))
    (with-current-buffer status-buffer-name
      (gitgit-get-branch-from-top-line))))

(provide 'gitgit)
