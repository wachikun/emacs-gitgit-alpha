;;; texe-interactive.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defun texe--comment (arg)
  "texe--comment"
  (interactive "*P")
  (let ((special-command-list (texe--get-region-special-begin-and-command)) comment-start)
    (if (and special-command-list
             (listp special-command-list)
             (string-match "elisp" (nth 0 special-command-list))
             (not (string-match "#@elisp" (buffer-substring (line-beginning-position)
                                                            (line-end-position)))))
        (setq comment-start ";;")
      (setq comment-start "#"))
    (comment-dwim arg)))

(defun texe--next-buffer ()
  (interactive)
  (let ((buffer-name (texe-get-next-buffer-name (texe--get-texe-buffer-list)
                                                (buffer-name))))
    (when (get-buffer buffer-name)
      (switch-to-buffer buffer-name))))

(defun texe--previous-buffer ()
  (interactive)
  (let ((buffer-name (texe-get-previous-buffer-name (texe--get-texe-buffer-list)
                                                    (buffer-name))))
    (when (get-buffer buffer-name)
      (switch-to-buffer buffer-name))))

(defun texe-rerun ()
  (interactive)
  (if texe-run-last-buffer
      (let ((last-buffer-point texe-process-local-run-last-buffer-point))
        (with-current-buffer texe-run-last-buffer
          (save-excursion
            (when last-buffer-point
              (goto-char last-buffer-point))
            (texe-run-internal t texe-run-last-force-yes-p))))
    (message "texe-run buffer not found")))

(defun texe-run ()
  (interactive)
  (texe-run-internal t nil))

(defun texe-run-internal (interactive-p force-yes-p)
  (catch 'error
    (let (special command special-command-list)
      (setq special-command-list (texe--get-region-special-begin-and-command))
      (if special-command-list
          (if (listp special-command-list)
              (progn
                (setq special (nth 0 special-command-list))
                (setq command (nth 1 special-command-list)))
            (message "illegal region")
            (throw 'error t))
        (setq special-command-list (texe--get-special-and-command))
        (if special-command-list
            (progn
              (setq special (nth 0 special-command-list))
              (setq command (nth 1 special-command-list)))
          (setq command (texe-get-line))))
      (if (and (not special)
               (string-match "^[ \t]*#" command))
          (message "comment line!")
        (when (string-prefix-p "#@FORCE-YES" special)
          (setq force-yes-p t))
        (if (or force-yes-p
                (yes-or-no-p (concat "run \"" command "\" ?")))
            (progn
              (when interactive-p
                (setq texe-run-last-buffer (buffer-name))
                (setq texe-run-last-force-yes-p force-yes-p))
              (texe--run-core special command "CSproc" t))
          (message "canceled!"))))))

(defun texe-process-mode-cancel-process ()
  (interactive)
  (when (and texe-process-local-process
             (eq 'run (process-status texe-process-local-process)))
    (delete-process texe-process-local-process)
    (setq texe-process-local-process nil)
    (texe-set-header-line-process-terminated)
    (message "PROCESS TERMINATED")))

(defun texe-process-mode-texe ()
  (interactive)
  (let ((buffer-name (texe-process-get-texe-buffer-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (message "texe buffer not found%s" texe-buffer-not-found-supplementary-message))))

(provide 'texe-interactive)
