;;; gitgit-setup.el -- Git User Interface -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defun gitgit-setup-modified-files-update-files ()
  (with-current-buffer (gitgit-status-get-status-buffer-name (buffer-name))
    (setq gitgit-status-local-modified-files-function
          'gitgit-status-after-save-hook-add-modified-files)))

(defun gitgit-setup-modified-files-update-status ()
  (with-current-buffer (gitgit-status-get-status-buffer-name (buffer-name))
    (setq gitgit-status-local-modified-files-function
          'gitgit-status-after-save-hook-rerun)))

(defun gitgit-setup (initial-directory rerun-p)
  (unless (string-match "/$" initial-directory)
    (setq initial-directory (concat initial-directory "/")))
  (setq initial-directory (expand-file-name initial-directory))
  (if (file-directory-p initial-directory)
      (let ((texe-buffer-name (gitgit--create-buffer-name gitgit-texe-file-name-prefix
                                                          initial-directory)))
        (if (texe-process-running-p (gitgit-status-get-process-back-buffer-name
                                     texe-buffer-name))
            (message "status running!")
          (if (and (get-buffer texe-buffer-name)
                   (get-buffer (gitgit-status-get-status-buffer-name texe-buffer-name)))
              (progn
                (gitgit--setup-after-second texe-buffer-name)
                (unless rerun-p
                  (let ((status-buffer-name (gitgit-status-get-status-buffer-name texe-buffer-name)))
                    (switch-to-buffer status-buffer-name))))
            (gitgit--setup-first texe-buffer-name initial-directory))))
    (message "gitgit.el : %s is not directory!"
             initial-directory)))

(defun gitgit-get-branch-from-top-line ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^## \\(.+\\)" nil t)
        (match-string 1)
      "?")))

(defun gitgit--create-buffer-name (prefix initial-directory)
  (let (buffer-name-base count)
    (string-match "/\\([^/]+\\)/$" initial-directory)
    (setq buffer-name-base (match-string 1 initial-directory))
    (if (gethash initial-directory gitgit--initial-directory-hash)
        (setq count (gethash initial-directory gitgit--initial-directory-hash))
      (let ((plus-one-count (1+ (gethash buffer-name-base gitgit--buffer-name-count-hash
                                         0))))
        (setq count plus-one-count)
        (puthash buffer-name-base plus-one-count gitgit--buffer-name-count-hash)
        (puthash initial-directory plus-one-count
                 gitgit--initial-directory-hash)))
    (concat prefix
            (match-string 1 initial-directory)
            (if (= count 1)
                ""
              (format " <%d>" count))
            "*")))

(defun gitgit--get-texe-full-path-name (initial-directory buffer-name)
  (let ((cooked-alist (mapcar #'(lambda (a)
                                  (cons (expand-file-name (car a)) (cdr a)))
                              gitgit-texe-alist)))
    (let ((texe-file (cdr (assoc initial-directory cooked-alist))))
      (if texe-file
          texe-file
        (string-match "^\\(.+?\\)\\( <[0-9]+>\\)?\\*$"
                      buffer-name)
        (concat gitgit-default-texe-directory
                (match-string 1 buffer-name)
                "*")))))

(defun gitgit--update-git-and-files-end-points ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" gitgit-texe-special-comment-end-of-git-status)
                           nil
                           t)
        (setq gitgit-status-local-end-of-git-status-point (point))
      (message "%s NOT FOUND" gitgit-texe-special-comment-end-of-git-status))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" gitgit-texe-special-comment-end-of-files)
                           nil
                           t)
        (setq gitgit-status-local-end-of-files-point (point))
      (message "%s NOT FOUND" gitgit-texe-special-comment-end-of-files))))

(defun gitgit--status-sentinel-callback-insert-recent-files ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" gitgit-texe-special-comment-recent-files)
                             nil
                             t)
      (forward-line 1)
      (beginning-of-line)
      (let ((backup-buffer-read-only buffer-read-only))
        (setq buffer-read-only nil)
        (mapc #'(lambda (value)
                  (insert (concat (nth 1 value)
                                  "\n")))
              (sort (hash-table-values gitgit-status-local-recent-files-hash)
                    #'(lambda (a b)
                        (time-less-p (nth 2 b)
                                     (nth 2 a)))))
        (setq buffer-read-only backup-buffer-read-only)))))

(defun gitgit--status-sentinel-callback ()
  "status sentinel callback
status $B@8@.(B (rerun) $B8e$K8F$P$l$k(B callback$B!#(B"
  (when (boundp 'gitgit-status-local-recent-files-hash)
    (gitgit--status-sentinel-callback-insert-recent-files))
  (texe-update-point)
  (texe-update-window-start texe-process-local-backup-point-alist)
  (let (backup-local-mark-hash backup-local-recent-files-hash
                               (backup-local-variable-list (texe-process-get-local-variable-list))
                               (branch (gitgit-get-branch-from-top-line)))
    (when (boundp 'gitgit-status-local-mark-hash)
      (setq backup-local-mark-hash gitgit-status-local-mark-hash))
    (when (boundp 'gitgit-status-local-recent-files-hash)
      (setq backup-local-recent-files-hash gitgit-status-local-recent-files-hash))
    (gitgit-status-mode)
    (with-current-buffer (gitgit-get-texe-buffer-name-from-related-buffer)
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat "^" gitgit-texe-special-comment-setup-modified-files-elisp
                                       "-begin")
                               nil
                               t)
            (texe-run t)
          (message "%s NOT FOUND"
                   (concat gitgit-texe-special-comment-setup-modified-files-elisp
                           "-begin"))))
      (gitgit-update-buffer-header-line branch))
    (gitgit-update-buffer-header-line branch)
    ;; $B>e$G(B major mode $B$r@_Dj$9$k$3$H$G(B buffer local $BJQ?t$,>C$($F$7$^$&$?$a!":F:n@.$7$F$$$k$3$H$KCm0U(B
    (texe-process-make-local-variable)
    (texe-process-update-local-variable-list backup-local-variable-list)
    (gitgit--update-git-and-files-end-points)
    (gitgit-status-update-faces)
    (when backup-local-mark-hash
      (gitgit-status-mark-hash backup-local-mark-hash))
    (when backup-local-recent-files-hash
      (setq gitgit-status-local-recent-files-hash
            backup-local-recent-files-hash))))

(defun gitgit--texe-sentinel-callback ()
  "texe sentinel callback
texe $B<B9T8e$K(B status $B$r99?7$9$k$?$a$N(B callback $B!#(B"
  (when (get-buffer (texe-process-get-texe-buffer-name))
    (with-current-buffer (texe-process-get-texe-buffer-name)
      (gitgit-status-sentinel-callback-rerun-status-from-texe))))

(defun gitgit--setup-texe-mode ()
  (texe-mode)
  (setq texe-buffer-not-found-supplementary-message
        " (rerun command M-x gitgit)")
  (setq texe-mode-local-run-core-special-alist (list (cons (concat gitgit-texe-special-comment-status-initialize-script
                                                                   "-begin") #'(lambda (special command)
                                                                   (let ((args-alist (list (cons 'no-display-process-buffer t))))
                                                                     (texe-run-start-process t
                                                                                             special
                                                                                             command
                                                                                             (gitgit-status-get-status-buffer-name (buffer-name))
                                                                                             args-alist
                                                                                             'gitgit--status-sentinel-callback))))
                                                     (cons (concat gitgit-texe-special-comment-setup-modified-files-elisp
                                                                   "-begin") #'(lambda (_special command)
                                                                   (eval (car (read-from-string command))))))))

(defun gitgit--draw-texe-buffer (texe-full-path-name)
  (unless (buffer-modified-p)
    (setq buffer-read-only nil)
    (buffer-disable-undo)
    (erase-buffer)
    (if (file-exists-p texe-full-path-name)
        (insert-file-contents texe-full-path-name)
      (insert gitgit-mode-default-texe))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun gitgit--run-initialize-scripts ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "^" gitgit-texe-special-comment-status-initialize-script
                                   "-begin")
                           nil
                           t)
        (texe-run t)
      (message "%s NOT FOUND"
               (concat gitgit-texe-special-comment-status-initialize-script
                       "-begin")))))

(defun gitgit--get-temporary-file-directory ()
  (expand-file-name (format ".emacs.gitgit.%d"
                            (user-uid))
                    temporary-file-directory))

(defun gitgit--setup-temporary-file-directory ()
  (unless gitgit-temporary-file-directory-for-visited-file-name
    (setq gitgit-temporary-file-directory-for-visited-file-name (gitgit--get-temporary-file-directory))
    (unless (file-accessible-directory-p gitgit-temporary-file-directory-for-visited-file-name)
      (make-directory gitgit-temporary-file-directory-for-visited-file-name)
      (set-file-modes gitgit-temporary-file-directory-for-visited-file-name
                      #o700))
    (unless (file-accessible-directory-p gitgit-temporary-file-directory-for-visited-file-name)
      (error "setup temporary file directory failed  directory=%s"
             gitgit-temporary-file-directory-for-visited-file-name))))

(defun gitgit--setup-first (texe-buffer-name initial-directory)
  (gitgit--setup-temporary-file-directory)
  (when (get-buffer texe-buffer-name)
    (kill-buffer texe-buffer-name))
  (let* ((status-buffer-name (gitgit-status-get-status-buffer-name texe-buffer-name))
         (texe-full-path-name (gitgit--get-texe-full-path-name initial-directory
                                                               texe-buffer-name)))
    (set-buffer (get-buffer-create texe-buffer-name))
    (set-visited-file-name texe-full-path-name
                           t)
    (rename-buffer texe-buffer-name)
    (set-buffer-modified-p nil)
    (setq default-directory initial-directory)
    (gitgit--draw-texe-buffer texe-full-path-name)
    (gitgit--setup-texe-mode)
    (gitgit--run-initialize-scripts)
    (add-hook 'texe-sentinel-callback-hook 'gitgit--texe-sentinel-callback)
    (switch-to-buffer status-buffer-name)))

(defun gitgit--setup-after-second (texe-buffer-name)
  (let (local-variable-found-p)
    (with-current-buffer texe-buffer-name
      (setq local-variable-found-p texe-mode-local-run-core-special-alist))
    (if local-variable-found-p
        (gitgit--setup-after-second-1 texe-buffer-name)
      (with-current-buffer texe-buffer-name
        (gitgit--setup-texe-mode))
      (gitgit--setup-after-second-1 texe-buffer-name)
      (message "REVERT DETECTED TEXE BUFFER"))))

(defun gitgit--setup-after-second-1 (texe-buffer-name)
  (with-current-buffer texe-buffer-name
    (gitgit--run-initialize-scripts)))

(provide 'gitgit-setup)
