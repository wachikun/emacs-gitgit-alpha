;;; gitgit-blame.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defface gitgit-blame--face-header '((((type x w32 mac ns)
                                       (class color)
                                       (background light))
                                      (:foreground "gray20" :bold t))
                                     (((type x w32 mac ns)
                                       (class color)
                                       (background dark))
                                      (:foreground "gray30" :bold t)))
  "header"
  :group 'gitgit-faces)

(defface gitgit-blame--face-information '((((type x w32 mac ns)
                                            (class color)
                                            (background light))
                                           (:foreground "gray20" :inherit default))
                                          (((type x w32 mac ns)
                                            (class color)
                                            (background dark))
                                           (:foreground "gray30" :inherit default)))
  "information"
  :group 'gitgit-faces)

(defconst gitgit-blame-mode-name " gitgit blame")
(defconst gitgit-blame-block-regexp "^\\([~^0-9a-f]+ .*?\\)\\(([^)]+ +[0-9]+) \\)")
(defconst gitgit-blame-revision-regexp "^\\([~^0-9a-f]+\\)")
(defconst gitgit-blame-file-name-regexp "^[~^0-9a-f]+ \\(.+?\\) ([^)]+ +[0-9]+) ")

(define-minor-mode gitgit-blame-mode
  "Toggle Bleme mode in the current buffer."
  :lighter gitgit-blame-mode-name
  :keymap '(("!" . gitgit-switch-to-texe)
            ("\C-c\C-v" . gitgit-switch-to-status)
            ("l" . gitgit-blame--log)
            ("\C-m" . gitgit-blame--cat)
            ("b" . gitgit-blame--blame)
            ("n" . gitgit-blame--next-line)
            ("p" . gitgit-blame--previous-line)
            (" " . gitgit-blame--scroll-page-forward)
            ("=" . gitgit-blame--diff)
            ("?" . gitgit-blame--show-hide-information)
            ("\t" . gitgit-blame--show-hide-information))(set (make-local-variable 'gitgit-blame-local--view-type)
            'gitgit-view-type-show-all-information))

(defun gitgit-blame-setup-buffer (&optional revision)
  (let ((file-name (nth 0
                        (cdr (assq 'file-list texe-process-local-args-alist))))
        (call-texe-buffer-name (cdr (assq 'i-texe-buffer-name texe-process-local-args-alist))))
    (setq texe-process-local-donot-touch-header-on-success
          t)
    (setq buffer-read-only nil)
    (gitgit-blame--move-information-text-to-overlay)
    (gitgit-status--sentinel-callback-not-rerun-status)
    (gitgit-blame--change-major-mode-and-keep-blame-mode (lambda ()
                                                           (set-visited-file-name (gitgit-blame-get-visited-file-name "blame"
                                                                                                                      file-name revision))))
    (gitgit-blame--update-buffer-invisibility-spec)
    (let ((buffer-name (concat call-texe-buffer-name
                               " "
                               (buffer-name))))
      (rename-buffer buffer-name))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (texe-set-point-min)))

(defun gitgit-blame-get-visited-file-name (prefix file-name &optional revision)
  (expand-file-name (format "%s.%s%s"
                            prefix
                            (if revision
                                (concat revision ".")
                              "")
                            (replace-regexp-in-string "[ /\\]" "_" file-name))
                    gitgit-temporary-file-directory-for-visited-file-name))

(defun gitgit-blame--get-revision ()
  (save-excursion
    (beginning-of-line)
    (if (eq 'gitgit-view-type-raw gitgit-blame-local--view-type)
        (if (re-search-forward gitgit-blame-revision-regexp
                               (line-end-position)
                               t)
            (match-string 1)
          nil)
      (let ((after-string (overlay-get (nth 0
                                            (overlays-in (point)
                                                         (point)))
                                       'after-string)))
        (if (string-match gitgit-blame-revision-regexp
                          after-string)
            (match-string 1 after-string)
          nil)))))

(defun gitgit-blame--get-file-name ()
  (save-excursion
    (beginning-of-line)
    (if (eq 'gitgit-view-type-raw gitgit-blame-local--view-type)
        (if (re-search-forward gitgit-blame-file-name-regexp
                               (line-end-position)
                               t)
            (match-string 1)
          (nth 0
               (cdr (assq 'file-list texe-process-local-args-alist))))
      (let ((after-string (overlay-get (nth 0
                                            (overlays-in (point)
                                                         (point)))
                                       'after-string)))
        (if (string-match gitgit-blame-file-name-regexp
                          after-string)
            (match-string 1 after-string)
          (nth 0
               (cdr (assq 'file-list texe-process-local-args-alist))))))))

(defun gitgit-blame--move-information-text-to-overlay ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward gitgit-blame-block-regexp
                              nil t)
      (let ((match-string-block-a (match-string 1))
            (match-string-block-b (match-string 2))
            overlay)
        (delete-region (line-beginning-position)
                       (point))
        (setq overlay (make-overlay (line-beginning-position)
                                    (line-beginning-position)))
        (overlay-put overlay
                     'after-string
                     (concat (propertize match-string-block-a
                                         'face
                                         'gitgit-blame--face-information
                                         'invisible
                                         '(gitgit-block-a))
                             (propertize match-string-block-b
                                         'face
                                         'gitgit-blame--face-information
                                         'invisible
                                         '(gitgit-block-b))))))))

(defun gitgit-blame--move-information-overlay-to-text ()
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((overlay (nth 0
                           (overlays-in (point)
                                        (point))))
             (after-string (overlay-get overlay 'after-string)))
        (delete-overlay overlay)
        (insert (substring-no-properties after-string))
        (beginning-of-line)
        (forward-line)))))

(defun gitgit-blame--update-header ()
  (setq header-line-format (propertize "? Show/Hide information    = diff    l log    b blame"
                                       'face 'gitgit-blame--face-header)))

(defun gitgit-blame--change-major-mode-and-keep-blame-mode (callback)
  (let ((copy-local-variable-list (texe-process-get-local-variable-list))
        (backup-local--view-type gitgit-blame-local--view-type))
    (funcall callback)
    (texe-process-make-local-variable)
    (texe-process-update-local-variable-list copy-local-variable-list)
    (gitgit-blame-mode)
    (gitgit-blame--update-header)
    (setq gitgit-blame-local--view-type backup-local--view-type)))

(defun gitgit-blame--run (command buffer-name-suffix sentinel-callback
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

(defun gitgit-blame--blame ()
  (interactive)
  (let* ((current-revision (substring-no-properties (gitgit-blame--get-revision)))
         (revision (completing-read "hash: "
                                    (list current-revision)
                                    nil
                                    nil
                                    current-revision))
         (file-name (gitgit-blame--get-file-name))
         (buffer (get-file-buffer (gitgit-blame-get-visited-file-name "blame"
                                                                      file-name revision)))
         command)
    (if buffer
        (pop-to-buffer buffer)
      (with-current-buffer (texe-process-get-texe-buffer-name)
        (setq command (gitgit-status--get-command "#@gitgit-status-blame"
                                                  nil)))
      (if (and revision file-name command)
          (gitgit-blame--run (concat command " " revision " -- " file-name)
                             (concat "blame-" revision "-" file-name)
                             (lambda ()
                               (gitgit-blame-setup-buffer revision))
                             file-name)
        (message "parameter not found")))))

(defun gitgit-blame--cat ()
  (interactive)
  (let ((revision (gitgit-blame--get-revision))
        (file-name (gitgit-blame--get-file-name))
        visited-file-name
        buffer)
    (if (and revision file-name)
        (progn
          (setq visited-file-name (gitgit-log--get-visited-file-name "cat" file-name
                                                                     revision))
          (setq buffer (get-file-buffer visited-file-name))
          (if buffer
              (pop-to-buffer buffer)
            (gitgit-blame--run (concat "git cat-file -p " revision ":" file-name)
                               "cat"
                               (lambda ()
                                 (texe-set-point-min)
                                 (set-visited-file-name (gitgit-blame-get-visited-file-name "cat"
                                                                                            file-name revision))
                                 (view-mode)
                                 (set-buffer-modified-p nil))
                               file-name)))
      (message "parameter not found"))))

(defun gitgit-blame--diff ()
  (interactive)
  (let ((revision (gitgit-blame--get-revision))
        (file-name (gitgit-blame--get-file-name))
        command)
    (with-current-buffer (texe-process-get-texe-buffer-name)
      (setq command (gitgit-status--get-command "#@gitgit-status-diff"
                                                nil)))
    (if (and revision file-name command)
        (gitgit-blame--run (concat command " " revision "~.." revision
                                   " -- " file-name)
                           (concat "diff-" revision "~.." revision "-"
                                   file-name)
                           (lambda ()
                             (texe-set-point-min)
                             (gitgit-diff-mode))
                           file-name)
      (message "parameter not found"))))

(defun gitgit-blame--update-buffer-invisibility-spec (&optional update-text-and-overlay-p)
  (cond
   ((eq 'gitgit-view-type-show-all-information
        gitgit-blame-local--view-type)
    (when update-text-and-overlay-p
      (setq buffer-read-only nil)
      (gitgit-blame--move-information-text-to-overlay)
      (gitgit-blame--change-major-mode-and-keep-blame-mode (lambda ()
                                                             (set-auto-mode)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))
    (setq buffer-invisibility-spec '()))
   ((eq 'gitgit-view-type-show-half-information
        gitgit-blame-local--view-type)
    (setq buffer-invisibility-spec '(gitgit-block-a)))
   ((eq 'gitgit-view-type-hide-information gitgit-blame-local--view-type)
    (setq buffer-invisibility-spec '(gitgit-block-a gitgit-block-b)))
   ((eq 'gitgit-view-type-raw gitgit-blame-local--view-type)
    (when update-text-and-overlay-p
      (setq buffer-read-only nil)
      (gitgit-blame--change-major-mode-and-keep-blame-mode (lambda ()
                                                             (fundamental-mode)))
      (gitgit-blame--move-information-overlay-to-text)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t))
    (setq buffer-invisibility-spec '(gitgit-block-a gitgit-block-b)))))

(defun gitgit-blame--show-hide-information ()
  (interactive)
  (cond
   ((eq 'gitgit-view-type-show-all-information
        gitgit-blame-local--view-type)
    (setq gitgit-blame-local--view-type 'gitgit-view-type-show-half-information))
   ((eq 'gitgit-view-type-show-half-information
        gitgit-blame-local--view-type)
    (setq gitgit-blame-local--view-type 'gitgit-view-type-hide-information))
   ((eq 'gitgit-view-type-hide-information gitgit-blame-local--view-type)
    (setq gitgit-blame-local--view-type 'gitgit-view-type-raw))
   ((eq 'gitgit-view-type-raw gitgit-blame-local--view-type)
    (setq gitgit-blame-local--view-type 'gitgit-view-type-show-all-information)))
  (gitgit-blame--update-buffer-invisibility-spec
   t))

(defun gitgit-blame--log ()
  (interactive)
  (let ((revision (gitgit-blame--get-revision)) command)
    (with-current-buffer (texe-process-get-texe-buffer-name)
      (setq command (gitgit-status--get-command "#@gitgit-status-log"
                                                nil)))
    (if revision
        (gitgit-blame--run (concat command " " revision)
                           "log"
                           (lambda ()
                             (texe-set-point-min)
                             (let ((copy-local-variable-list (texe-process-get-local-variable-list)))
                               (gitgit-log-mode)
                               (texe-process-make-local-variable)
                               (texe-process-update-local-variable-list copy-local-variable-list)))
                           nil)
      (message "revision not found"))))

(defun gitgit-blame--next-line ()
  (interactive)
  (forward-line 1))

(defun gitgit-blame--previous-line ()
  (interactive)
  (forward-line -1))

(defun gitgit-blame--scroll-page-forward ()
  (interactive)
  (scroll-up (window-body-height)))

(provide 'gitgit-blame)
