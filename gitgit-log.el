;;; gitgit-log.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defcustom gitgit-log--default-face-limit-float-time
  0.5 "gitgit-log--default-face-limit-float-time"
  :type 'float
  :group 'gitgit-variables)

(defvar gitgit-log-mode-map (make-sparse-keymap))

(defface gitgit-log--face-revision '((((type x w32 mac ns)
                                       (class color)
                                       (background light))
                                      (:foreground "#00007f" :background "lightgray"
                                                   :bold t
                                                   :underline t))
                                     (((type x w32 mac ns)
                                       (class color)
                                       (background dark))
                                      (:foreground "#00007f" :background "gray"
                                                   :bold t
                                                   :underline t)))
  "log revision"
  :group 'gitgit-faces)

(defface gitgit-log--face-file-name '((((type x w32 mac ns)
                                        (class color)
                                        (background light))
                                       (:foreground "#00007f" :underline t))
                                      (((type x w32 mac ns)
                                        (class color)
                                        (background dark))
                                       (:foreground "#00007f" :underline t)))
  "log file-name"
  :group 'gitgit-faces)

(defconst gitgit-log-mode-name "gitgit log")
(defconst gitgit-log-revision-regexp "^[\\*\\|\\\\ ]*\\(commit\\) +\\([0-9a-f]+\\)")
(defconst gitgit-log-file-name-regexp "^\\([| ]+\\)\\(.+?\\) +| +\\(?:[a-zA-Z]+ +[0-9]+ +-> +[0-9]+ bytes\\|[0-9]+\\(?: [-+]+\\)?\\)?$")

(define-derived-mode gitgit-log-mode
  nil
  gitgit-log-mode-name
  "Major mode for gitgit log"
  (define-key gitgit-log-mode-map "\C-i" 'gitgit-log--next-file-name)
  (define-key gitgit-log-mode-map "g" 'texe-rerun)
  (define-key gitgit-log-mode-map "\C-c\C-c"
              'texe-rerun)
  (define-key gitgit-log-mode-map "n" 'gitgit-log--next)
  (define-key gitgit-log-mode-map "p" 'gitgit-log--previous)
  (define-key gitgit-log-mode-map " " 'scroll-up)
  (define-key gitgit-log-mode-map "\C-m" 'gitgit-log--enter)
  (define-key gitgit-log-mode-map "=" 'gitgit-log--diff)
  (define-key gitgit-log-mode-map "!" 'gitgit-switch-to-texe)
  (define-key gitgit-log-mode-map "\C-c\C-v"
              'gitgit-switch-to-status)
  (define-key gitgit-log-mode-map "\M-." 'gitgit-status--next-status-buffer)
  (define-key gitgit-log-mode-map "\M-," 'gitgit-status--previous-status-buffer)
  (setq buffer-read-only nil)
  (gitgit-update-buffer-header-line (gitgit-get-current-branch))
  (gitgit-log-update-faces)
  (setq buffer-read-only t))

(defun gitgit-log-update-faces ()
  (let ((start-float-time (float-time)))
    (save-excursion
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (< (- (float-time)
                        start-float-time) gitgit-log--default-face-limit-float-time))
        (if (gitgit-log--search-revision-line (line-end-position))
            (set-text-properties (line-beginning-position)
                                 (line-end-position)
                                 (list 'face 'gitgit-log--face-revision))
          (beginning-of-line)
          (when (re-search-forward gitgit-log-file-name-regexp
                                   (line-end-position)
                                   t)
            (set-text-properties (+ (length (match-string 1))
                                    (line-beginning-position))
                                 (+ (length (match-string 1))
                                    (length (match-string 2))
                                    (line-beginning-position))
                                 (list 'face 'gitgit-log--face-file-name)))
          (forward-line 1))))))

(defun gitgit-log--run (command buffer-name-suffix sentinel-callback
                                file-name)
  (let ((args-alist (list (cons 'file-list (if file-name
                                               (list file-name)
                                             nil)))))
    (with-current-buffer (texe-process-get-texe-buffer-name)
      (texe-run-start-process nil
                              nil
                              command
                              (concat (buffer-name)
                                      " "
                                      buffer-name-suffix)
                              args-alist
                              sentinel-callback))))

(defun gitgit-log--get-visited-file-name (prefix file-name &optional revision)
  (expand-file-name (format "gitgit-log.%s.%s%s"
                            prefix
                            (if revision
                                (concat revision ".")
                              "")
                            (replace-regexp-in-string "[ /\\]" "_" file-name))
                    gitgit-temporary-file-directory-for-visited-file-name))

(defun gitgit-log--get-revision ()
  (save-excursion
    (if (gitgit-log--search-revision-line-backward
         nil)
        (match-string 2)
      nil)))

(defun gitgit-log--get-file-name ()
  (save-excursion
    (if (re-search-forward gitgit-log-file-name-regexp
                           (line-end-position)
                           t)
        (match-string 2)
      nil)))

(defun gitgit-log--search-revision-line (limit)
  (re-search-forward gitgit-log-revision-regexp
                     limit t))

(defun gitgit-log--search-revision-line-backward (limit)
  (re-search-backward gitgit-log-revision-regexp
                      limit t))

(defun gitgit-log--search-file-name-line (limit)
  (re-search-forward gitgit-log-file-name-regexp
                     limit t))

(defun gitgit-log--next-file-name ()
  (interactive)
  (let ((backup (point)))
    (forward-line)
    (if (not (gitgit-log--search-file-name-line nil))
        (goto-char backup)
      (beginning-of-line))))

(defun gitgit-log--next ()
  (interactive)
  (let ((backup (point)))
    (forward-line)
    (if (not (gitgit-log--search-revision-line nil))
        (goto-char backup)
      (beginning-of-line)
      (recenter 0))))

(defun gitgit-log--previous ()
  (interactive)
  (gitgit-log--search-revision-line-backward
   nil)
  (beginning-of-line)
  (recenter 0))

(defun gitgit-log--enter (arg)
  (interactive "P")
  (if arg
      (gitgit-log--cat)
    (let ((file-name (gitgit-log--get-file-name)))
      (when file-name
        (find-file-other-window file-name)))))

(defun gitgit-log--cat ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((revision (gitgit-log--get-revision))
          (file-name (gitgit-log--get-file-name))
          visited-file-name
          buffer)
      (if (and revision file-name)
          (progn
            (setq visited-file-name (gitgit-log--get-visited-file-name "cat" file-name
                                                                       revision))
            (setq buffer (get-file-buffer visited-file-name))
            (if buffer
                (pop-to-buffer buffer)
              (gitgit-log--run (concat "git cat-file -p " revision ":" file-name)
                               "cat"
                               #'(lambda ()
                                   (texe-set-point-min)
                                   (set-visited-file-name visited-file-name)
                                   (view-mode)
                                   (set-buffer-modified-p nil))
                               file-name)))
        (message "parameter not found")))))

(defun gitgit-log--diff ()
  (interactive)
  (save-excursion
    (let ((limit (point)))
      (end-of-line)
      (setq limit (point))
      (beginning-of-line)
      (if (gitgit-log--search-revision-line limit)
          (let ((revision (match-string 2))
                (status-buffer-name (gitgit-status-get-status-buffer-name (gitgit-get-texe-buffer-name-from-related-buffer))))
            (with-current-buffer status-buffer-name
              (gitgit-status--run nil
                                  "diff"
                                  "diff"
                                  "diff"
                                  'gitgit-status--sentinel-callback-not-rerun-status
                                  (list (concat revision "~")
                                        revision))))
        (message "not found")))))

(provide 'gitgit-log)
