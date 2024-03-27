;;; texe.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

;; args-alist
;; 
;; - file-list
;;     command 構築前の file list 。
;; - script-tmpfile
;;     script 実行用の tmpfile 。
;; - i-texe-buffer-name
;;     texe-run-start-process で設定される。
;; - i-from-texe
;;     texe から起動した場合に設定される。

;; https://github.com/wachikun/emacs-gitgit-alpha

;;; Code:

(require 'texe-vars)
(require 'texe-interactive)
(require 'texe-run-start-process)
(require 'texe-point)
(require 'texe-process)
(require 'texe-specials)

(define-minor-mode texe-mode
  "Toggle Texe mode in the current buffer."
  :lighter texe--mode-name
  (set (make-local-variable 'texe-mode-local-run-core-special-alist)
       nil)
  (set (make-local-variable 'texe-mode-local-default-special-regexp-list)
       nil)
  (font-lock-add-keywords nil
                          '(("^[ \t]*\\(#@.*\\)" 1 'texe--face-special-comment)
                            ("^[ \t]*\\(#[^@].*\\)" 1 'texe--face-comment)
                            ("^[ \t]*\\(;.*\\)" 1 'font-lock-comment-face)))
  (texe-l-setup-default-special-regexp)
  (if texe-mode
      (use-local-map texe-mode-map)
    (use-local-map nil)))

(defun texe-get-line ()
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(defun texe-set-line-face-if-need ()
  (let* ((line (texe-get-line))
         (face (cond
                ((string-match "^#@" line) 'texe--face-special-comment)
                ((string-match "^#" line) 'texe--face-comment)
                (t nil))))
    (when face
      (let ((backup-buffer-read-only buffer-read-only))
        (setq buffer-read-only nil)
        (add-text-properties (line-beginning-position)
                             (line-end-position)
                             (list 'face face))
        (setq buffer-read-only backup-buffer-read-only)))))

(defun texe-get-next-buffer-name (buffer-name-list buffer-name)
  (let ((result (cdr (member buffer-name buffer-name-list))))
    (if result
        (car result)
      (nth 0 buffer-name-list))))

(defun texe-get-previous-buffer-name (buffer-name-list buffer-name)
  (let* ((list-length (length buffer-name-list))
         (membered-list (member buffer-name buffer-name-list))
         (membered-length (length membered-list))
         (previous-index (1- (- list-length membered-length))))
    (if (< previous-index 0)
        (nth (1- list-length)
             buffer-name-list)
      (nth previous-index buffer-name-list))))

(defun texe-get-current-line-command ()
  (catch 'error
    (let (command special-command-list)
      (setq special-command-list (texe--get-region-special-begin-and-command))
      (if special-command-list
          (if (listp special-command-list)
              (setq command (nth 1 special-command-list))
            (message "illegal region")
            (throw 'error nil))
        (setq special-command-list (texe--get-special-and-command))
        (if special-command-list
            (setq command (nth 1 special-command-list))
          (setq command (texe-get-line))))
      command)))

(defun texe--get-region-special-begin-and-command ()
  "#@*-begin
#@*-end
で囲まれていれば (\"#@*-begin\" \"command\") のリストを、囲まれていなければ nil を、 #@* が
一致していなければ t 返す。"
  (catch 'return
    (let (special-begin special-end command-start command)
      (save-excursion
        (when (string-match "^#@.+-end$" (texe-get-line))
          (forward-line -1)
          (when (string-match "^#@.+-end$" (texe-get-line))
            (throw 'return nil)))
        (when (string-match "^#@.+-begin" (texe-get-line))
          (end-of-line))
        (when (re-search-backward "^#@" nil t)
          (setq special-begin (texe-get-line))
          (when (string-match ".-begin" special-begin)
            (forward-line 1)
            (setq command-start (point))
            (when (re-search-forward "^#@" nil t)
              (setq special-end (texe-get-line))
              (when (string-match ".-end$" special-end)
                (let (special-begin-special special-end-special)
                  (string-match "^([^-]+)-" special-begin)
                  (setq special-begin-special (match-string 1 special-begin))
                  (string-match "^([^-]+)-" special-end)
                  (setq special-end-special (match-string 1 special-end))
                  (if (string= special-begin-special special-end-special)
                      (progn
                        (setq command (buffer-substring command-start
                                                        (line-beginning-position)))
                        (list special-begin command))
                    t))))))))))

(defun texe--get-special-and-command ()
  "現在の行が #@* の次か #@* と同じならば (\"#@*\" \"command\") のリストを、そうでなければ
nil を返す。但し、 #@* が -begin または -end で終えていた場合も nil を返す"
  (save-excursion
    (let (tmp special command)
      (setq tmp (texe-get-line))
      (if (and (string-match "^#@" tmp)
               (not (string-match ".-begin$" tmp))
               (not (string-match ".-end$" tmp)))
          (progn
            (setq special tmp)
            (forward-line 1)
            (setq command (texe-get-line))
            (list special command))
        (forward-line -1)
        (setq special (texe-get-line))
        (when (and (string-match "^#@" special)
                   (not (string-match ".-begin$" special))
                   (not (string-match ".-end$" special)))
          (setq command tmp)
          (list special command))))))

(defun texe-l-setup-default-special-regexp ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat texe--special-comment-regexp-special-regexp
                                     "begin")
                             nil
                             t)
      (beginning-of-line)
      (let ((special-command-list (texe--get-region-special-begin-and-command)))
        (if (and special-command-list
                 (listp special-command-list))
            (let* ((command (replace-regexp-in-string "\n$"
                                                      ""
                                                      (nth 1 special-command-list)))
                   (default-special-regexp-list (split-string command "\n")))
              (if (= (% (length default-special-regexp-list)
                        2) 0)
                  (setq texe-mode-local-default-special-regexp-list
                        default-special-regexp-list)
                (message "illegal pair")
                (throw 'error nil)))
          (message "illegal region")
          (throw 'error nil))))))

(provide 'texe)
