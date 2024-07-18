;;; gitgit-status-interactive.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defun gitgit-status--mark-unknown ()
  (interactive)
  (gitgit-status--mark-mark "?"))

(defun gitgit-status--mark-add ()
  (interactive)
  (gitgit-status--mark-mark "A"))

(defun gitgit-status--mark-remove ()
  (interactive)
  (gitgit-status--mark-mark "D"))

(defun gitgit-status--mark-modified ()
  (interactive)
  (gitgit-status--mark-mark "M"))

(defun gitgit-status--mark-path-regexp ()
  (interactive)
  (let ((regexp (completing-read "mark regexp: "
                                 '(("Makefile$")
                                   ("\.cpp$")))))
    (save-excursion
      (goto-char (point-min))
      (while (gitgit-status--git-re-search-forward)
        (let ((file-name (match-string 2)))
          (when (string-match regexp file-name)
            (gitgit-status--mark)))))))

(defun gitgit-status--unmark-all ()
  (interactive)
  (save-excursion
    (maphash #'(lambda (key _value)
                 (goto-char (point-min))
                 (when (gitgit-status--mark-re-search-forward key)
                   (gitgit-status--unmark)))
             gitgit-status-local-mark-hash))
  (clrhash gitgit-status-local-mark-hash))

(defun gitgit-status--get-status-buffer-list ()
  (let (status-buffer-list)
    (mapc #'(lambda (buffer)
              (with-current-buffer buffer
                (when (eq major-mode 'gitgit-status-mode)
                  (setq status-buffer-list (append status-buffer-list
                                                   (list (buffer-name)))))))
          (buffer-list))
    (sort status-buffer-list 'string<)))

(defun gitgit-status--next-status-buffer ()
  (interactive)
  (let ((buffer-name (texe-get-next-buffer-name (gitgit-status--get-status-buffer-list)
                                                (buffer-name))))
    (when (get-buffer buffer-name)
      (switch-to-buffer buffer-name))))

(defun gitgit-status--previous-status-buffer ()
  (interactive)
  (let ((buffer-name (texe-get-previous-buffer-name (gitgit-status--get-status-buffer-list)
                                                    (buffer-name))))
    (when (get-buffer buffer-name)
      (switch-to-buffer buffer-name))))

(defun gitgit-status--view-status-from-texe-buffer (arg)
  (interactive "P")
  (let ((buffer-name (gitgit-status-get-status-buffer-name (buffer-name))))
    (when (get-buffer buffer-name)
      (if arg
          (pop-to-buffer buffer-name)
        (switch-to-buffer buffer-name)))))

(defun gitgit-status--mkdir (directory)
  (interactive "FCreate directory: ")
  (make-directory directory))

(defun gitgit-status--run-commit ()
  (interactive)
  (gitgit-status--run-commit-1 nil "commit"
                               "commit" 'gitgit-status--sentinel-callback-rerun-status
                               nil))

(defun gitgit-status--run-find-file ()
  (interactive)
  (let ((line (texe-get-line)))
    (unless (gitgit-status--ignore-p line)
      (let ((file-name (gitgit-status--get-current-file-name 'gitgit-status--can-find-file)))
        (when (file-exists-p file-name)
          (find-file file-name))))))

(defun gitgit-status--mark ()
  (interactive)
  (unless (gitgit-status--ignore-current-line-p)
    (gitgit-status--mark-current-line)
    (gitgit-status--mark-line-face t))
  (forward-line 1))

(defun gitgit-status--unmark ()
  (interactive)
  (unless (gitgit-status--ignore-current-line-p)
    (gitgit-status--unmark-current-line)
    (gitgit-status--mark-line-face nil))
  (forward-line 1))

(defun gitgit-status--previous-line ()
  (interactive)
  (catch 'loop
    (while (> (point) (point-min))
      (forward-line -1)
      (unless (gitgit-status--ignore-current-line-p)
        (throw 'loop nil)))))

(defun gitgit-status--next-line ()
  (interactive)
  (catch 'loop
    (while (< (point) (point-max))
      (forward-line 1)
      (unless (gitgit-status--ignore-current-line-p)
        (throw 'loop nil)))))

(defun gitgit-status--rerun-status ()
  (with-current-buffer (gitgit-get-texe-buffer-name-from-related-buffer)
    (with-current-buffer (gitgit-status-get-status-buffer-name (buffer-name))
      (gitgit-status--rerun-process))))

(defun gitgit-status--rerun-process ()
  (interactive)
  (if gitgit-status-local-ignore-rerun
      (message "Can not rerun")
    (clrhash gitgit-status-local-after-save-hash)
    (gitgit default-directory t)))

(defun gitgit-status--git-add ()
  (interactive)
  (let ((file-list (gitgit-status--get-mark-file-names-or-current-file-name
                    'gitgit-status--can-add)))
    (when file-list
      (gitgit-status-set-header "ADD")
      (gitgit-status--run t "add" "add" "add" 'gitgit-status--sentinel-callback-rerun-status-kill-process-buffer
                          file-list))))

(defun gitgit-status--git-blame ()
  (interactive)
  (let* ((file-name (gitgit-status--get-current-file-name 'gitgit-status--can-blame))
         (file-list (if file-name
                        (list file-name)
                      nil)))
    (when file-list
      (let ((buffer (get-file-buffer (gitgit-blame-get-visited-file-name "blame"
                                                                         (nth 0 file-list)))))
        (if buffer
            (pop-to-buffer buffer)
          (gitgit-status--run nil
                              "blame"
                              "blame"
                              "blame"
                              #'(lambda ()
                                  (gitgit-blame-setup-buffer))
                              file-list))))))

(defun gitgit-status--git-remove ()
  (interactive)
  (let ((file-list (gitgit-status--get-mark-file-names-or-current-file-name
                    'gitgit-status--can-remove)))
    (when file-list
      (gitgit-status-set-header "REMOVE")
      (gitgit-status--run t "rm" "rm" "remove" 'gitgit-status--sentinel-callback-rerun-status-kill-process-buffer
                          file-list))))

(defun gitgit-status--git-diff-current-file (arg)
  (interactive "P")
  (let ((current-staged-file-name (gitgit-status--get-current-file-name 'gitgit-status--can-diff-staged))
        (diff (if arg "diff" "diff --staged"))
        file-list)
    (if current-staged-file-name
        (push current-staged-file-name file-list)
      (setq diff "diff")
      (when (gitgit-status--get-current-file-name 'gitgit-status--can-diff)
        (push (gitgit-status--get-current-file-name 'gitgit-status--can-diff)
              file-list)))
    (gitgit-status--run nil "diff" diff "diff"
                        'gitgit-status--sentinel-callback-not-rerun-status
                        file-list)))

(defun gitgit-status--git-diff-mark-files (arg)
  (interactive "P")
  (let ((staged-files (gitgit-status--get-mark-file-names-or-current-file-name
                       'gitgit-status--can-diff-staged))
        (not-staged-files (gitgit-status--get-mark-file-names-or-current-file-name
                           'gitgit-status--can-diff-not-staged))
        (diff (if arg "diff" "diff --staged"))
        file-list
        staged-p)
    (if (and staged-files not-staged-files)
        (setq staged-p (yes-or-no-p "diff staged?"))
      (setq staged-p staged-files))
    (if staged-p
        (setq file-list staged-files)
      (setq file-list not-staged-files)
      (setq diff "diff"))
    (gitgit-status--run nil "diff" diff "diff"
                        'gitgit-status--sentinel-callback-not-rerun-status
                        file-list)))

(defun gitgit-status--git-log (arg)
  (interactive "P")
  (let ((file-list (if (gitgit-status--mark-file-names-p)
                       (gitgit-status--get-mark-file-names-or-current-file-name
                        'gitgit-status--can-log)
                     nil)))
    (gitgit-status--run nil
                        "log"
                        "log"
                        "log"
                        'gitgit-status--sentinel-callback-not-rerun-status
                        file-list
                        #'(lambda (command)
                            (if arg
                                (replace-regexp-in-string " -n [0-9]+"
                                                          (format " -n %d"
                                                                  (prefix-numeric-value arg))
                                                          command)
                              command)))))

(defun gitgit-status--git-auto-revert-restore (line)
  (cond
   ((string-match (concat "^\\(.[ADMR]\\)" gitgit-status--file-name-regexp) line)
    (cons (match-string 1 line) (match-string 2 line)))
   (t nil)))

(defun gitgit-status--git-auto-revert-restore-staged (line file-list-restore)
  (cond
   ((string-match (concat "^\\([DMR].\\)" gitgit-status--file-name-regexp) line)
    (if (member (match-string 2 line) file-list-restore)
        nil
      (cons (match-string 1 line) (match-string 2 line))))
   (t nil)))

(defun gitgit-status--git-auto-revert-rm-cached (line file-list-restore)
  (cond
   ((string-match (concat "^\\(A.\\)" gitgit-status--file-name-regexp) line)
    (if (member (match-string 2 line) file-list-restore)
        nil
      (cons (match-string 1 line) (match-string 2 line))))
   (t nil)))

(defun gitgit-status--git-auto-revert ()
  (interactive)
  (let* ((file-list-restore (gitgit-status--get-mark-file-names-or-current-file-name
                             'gitgit-status--git-auto-revert-restore))
         (file-list-restore-staged (gitgit-status--get-mark-file-names-or-current-file-name #'(lambda (line)
                                                                                                (gitgit-status--git-auto-revert-restore-staged
                                                                                                 line file-list-restore))))
         (file-list-rm-cached (gitgit-status--get-mark-file-names-or-current-file-name #'(lambda (line)
                                                                                           (gitgit-status--git-auto-revert-rm-cached
                                                                                            line file-list-restore)))))
    (when (or file-list-restore file-list-restore-staged
              file-list-rm-cached)
      (let ((message ""))
        (when file-list-restore
          (setq message (concat message
                                "restore \""
                                (mapconcat #'(lambda (s)
                                               (shell-quote-argument s))
                                           file-list-restore
                                           " ")
                                "\" ?")))
        (when file-list-restore-staged
          (unless (string= message "")
            (setq message (concat message "\n")))
          (setq message (concat message
                                "restore --staged \""
                                (mapconcat #'(lambda (s)
                                               (shell-quote-argument s))
                                           file-list-restore-staged
                                           " ")
                                "\" ?")))
        (when file-list-rm-cached
          (unless (string= message "")
            (setq message (concat message "\n")))
          (setq message (concat message
                                "rm --cached \""
                                (mapconcat #'(lambda (s)
                                               (shell-quote-argument s))
                                           file-list-rm-cached
                                           " ")
                                "\" ?")))
        (if (yes-or-no-p message)
            (progn
              (gitgit-status-set-header "AUTO REVERT")
              (when file-list-restore
                (gitgit-status--run t "restore" "restore"
                                    "auto-revert-restore" 'gitgit-status--sentinel-callback-rerun-status-kill-process-buffer
                                    file-list-restore))
              (when file-list-restore-staged
                (gitgit-status--run t "restore" "restore --staged"
                                    "auto-revert-restore--staged" 'gitgit-status--sentinel-callback-rerun-status-kill-process-buffer
                                    file-list-restore-staged))
              (when file-list-rm-cached
                (gitgit-status--run t "rm" "rm --cached" "auto-revert-rm--cached"
                                    'gitgit-status--sentinel-callback-rerun-status-kill-process-buffer
                                    file-list-rm-cached)))
          (message "canceled!"))))))

(defun gitgit-status--git-rename (destination)
  (interactive (let* ((insert-default-directory nil)
                      (file-list (gitgit-status--get-mark-file-names-or-current-file-name
                                  'gitgit-status--can-rename))
                      (file-list-length (length file-list)))
                 (list (cond
                        ((eq file-list nil) nil)
                        ((<= file-list-length 1)
                         (read-file-name (format "rename \"%s\" to: " file-list)))
                        (t (read-file-name (format "rename %d files to: " file-list-length)))))))
  (let ((file-list (gitgit-status--get-mark-file-names-or-current-file-name
                    'gitgit-status--can-rename)))
    (setq file-list (append file-list
                            (list destination)))
    (if file-list
        (progn
          (gitgit-status-set-header "RENAME")
          (gitgit-status--run t "mv" "mv" "mv" 'gitgit-status--sentinel-callback-rerun-status-kill-process-buffer
                              file-list))
      (message "file-name not found"))))

(defun gitgit-status--quit ()
  (interactive)
  (if (yes-or-no-p (concat "quit? "))
      (let ((search-buffer-name (replace-regexp-in-string (concat " " gitgit-texe-status-buffer-suffix
                                                                  "$")
                                                          ""
                                                          (buffer-name))))
        (mapc #'(lambda (a)
                  (let ((buffer-name (buffer-name a)))
                    (when (string-match (concat "^"
                                                (regexp-quote search-buffer-name)) buffer-name)
                      (kill-buffer buffer-name))))
              (buffer-list)))
    (message "canceled!")))

(defun gitgit-status--mark-mark (search-mark)
  (save-excursion
    (goto-char (point-min))
    (while (gitgit-status--git-re-search-forward)
      (let ((mark (match-string 1)))
        (when (string-match search-mark mark)
          (gitgit-status--mark))))))

(defun gitgit-status--mark-file-names-p ()
  (> (hash-table-count gitgit-status-local-mark-hash) 0))

(defun gitgit-status--get-current-file-name (can-do-function)
  (let ((line (texe-get-line)))
    (if (gitgit-status--ignore-p line)
        nil
      (let ((file-name (funcall can-do-function line)))
        (if file-name
            ;; git status は escape され、 ls 系は escape されないので読み分けていることに注意
            (if (<= (point) gitgit-status-local-end-of-git-status-point)
                (gitgit-status--remove-git-escape (cdr file-name))
              (cdr file-name))
          nil)))))

(defun gitgit-status--remove-git-escape (file-name)
  (replace-regexp-in-string "\\\\"
                            ""
                            (replace-regexp-in-string "^\"\\(.+\\)\"$"
                                                      "\\1" file-name)))

(defun gitgit-status--get-mark-file-names-or-current-file-name (can-do-function)
  (let (file-list)
    (if (> (hash-table-count gitgit-status-local-mark-hash) 0)
        (maphash #'(lambda (key _value)
                     (unless (gitgit-status--ignore-p key)
                       (let ((file-name (funcall can-do-function key)))
                         (when file-name
                           (add-to-list 'file-list
                                        (gitgit-status--remove-git-escape (cdr file-name)))))))
                 gitgit-status-local-mark-hash)
      (let ((file-name (gitgit-status--get-current-file-name can-do-function)))
        (when file-name
          (push file-name file-list))))
    file-list))

(defun gitgit-status--run-commit-1 (no-display-process-buffer-p git-command buffer-name-suffix
                                                                sentinel-callback &optional file-list command-filter)
  (let* ((commit-buffer-name (concat (gitgit-get-texe-buffer-name-from-related-buffer)
                                     " "
                                     buffer-name-suffix))
         (log-buffer-name (concat commit-buffer-name "-log"))
         (branch (gitgit-get-branch-from-top-line)))
    (save-excursion
      (gitgit-status-set-header "COMMIT")
      (gitgit-status--run-1 no-display-process-buffer-p
                            git-command git-command commit-buffer-name
                            sentinel-callback file-list command-filter
                            t))
    (gitgit-status--run-commit-1-show-log-buffer
     log-buffer-name branch)))

(defun gitgit-status--run-commit-1-show-log-buffer (buffer-name branch)
  (gitgit-status--run-1 nil "commit-information-log"
                        "log" buffer-name 'gitgit-status--sentinel-callback-not-rerun-status
                        nil nil t))

(defun gitgit-status--mark-re-search-forward (file-name)
  "file-name を持つ git files または files を re-search-forward する"
  (if (string-match "^.. \\(.+\\)" file-name)
      (re-search-forward (concat "^.. "
                                 (regexp-quote (match-string 1 file-name)))
                         gitgit-status-local-end-of-git-status-point
                         t)
    (re-search-forward (concat "^"
                               (regexp-quote file-name))
                       gitgit-status-local-end-of-files-point
                       t)))

(defun gitgit-status--git-mark-re-search-forward (file-name)
  "file-name を持つ git files を re-search-forward する
直後の (match-string 1) で mark を取得できる。"
  (when (string-match "^.. \\(.+\\)" file-name)
    (re-search-forward (concat "^\\(..\\) "
                               (regexp-quote (match-string 1 file-name)))
                       gitgit-status-local-end-of-git-status-point
                       t)))

(defun gitgit-status--git-re-search-forward ()
  "current 位置以降の git files を re-search-forward する
直後の (match-string 1) で mark を、 (match-string 2) で file-name を取得できる。"
  (re-search-forward "^\\(..\\) \\(.+\\)" gitgit-status-local-end-of-git-status-point
                     t))

(defun gitgit-status--mark-line-face (mark-p)
  (let ((backup-buffer-read-only buffer-read-only)
        (face (if (<= (point) gitgit-status-local-end-of-git-status-point)
                  (if (string-match "^[^ ?]" (texe-get-line))
                      'gitgit-status--face-git-staged-mark
                    'gitgit-status--face-git-mark)
                'gitgit-status--face-git-mark)))
    (setq buffer-read-only nil)
    (if mark-p
        (add-text-properties (line-beginning-position)
                             (line-end-position)
                             (list 'face face))
      (remove-text-properties (line-beginning-position)
                              (line-end-position)
                              (list 'face nil))
      (gitgit-status--set-file-line-face))
    (setq buffer-read-only backup-buffer-read-only)))

(defun gitgit-status--set-file-line-face ()
  (let ((backup-buffer-read-only buffer-read-only)
        (face (if (<= (point) gitgit-status-local-end-of-git-status-point)
                  (if (string-match "^[^ ?]" (texe-get-line))
                      'gitgit-status--face-git-staged-files
                    'gitgit-status--face-git-status-files)
                'gitgit-status--face-files)))
    (setq buffer-read-only nil)
    (when (gitgit-status--can-default (texe-get-line))
      (add-text-properties (line-beginning-position)
                           (line-end-position)
                           (list 'face face)))
    (setq buffer-read-only backup-buffer-read-only)))

(defun gitgit-status--mark-current-line ()
  (puthash (texe-get-line)
           "*"
           gitgit-status-local-mark-hash))

(defun gitgit-status--unmark-current-line ()
  (remhash (texe-get-line)
           gitgit-status-local-mark-hash))

(defun gitgit-status--ignore-p (line)
  (string-match "^\\($\\|[\t ]*#\\)" line))

(defun gitgit-status--ignore-current-line-p ()
  (if (<= (point) gitgit-status-local-end-of-files-point)
      (gitgit-status--ignore-p (texe-get-line))
    t))

(defun gitgit-status--can-default (line)
  (cond
   ((string-match "^\\([^#].\\) \\(.+\\)" line)
    (cons (match-string 1 line) (match-string 2 line)))
   ((string-match "^\\([^#]+\\)" line)
    (cons nil (match-string 1 line)))
   (t nil)))

(defun gitgit-status--can-find-file (line)
  (cond
   ((string-match "^\\(..\\) .+? -> \\(.+\\)$"
                  line)
    (cons (match-string 1 line) (match-string 2 line)))
   ((string-match "^\\([^#].\\) \\(.+\\)" line)
    (cons (match-string 1 line) (match-string 2 line)))
   ((string-match "^\\([^#]+\\)" line)
    (cons nil (match-string 1 line)))
   (t nil)))

(defun gitgit-status--can-add (line)
  (cond
   ((string-match "^\\(.M\\) .+? -> \\(.+\\)$"
                  line)
    (cons (match-string 1 line) (match-string 2 line)))
   ((string-match "^\\([^#].\\) \\(.+\\)" line)
    (cons (match-string 1 line) (match-string 2 line)))
   ((string-match "^\\([^#]+\\)" line)
    (cons nil (match-string 1 line)))
   (t nil)))

(defun gitgit-status--can-blame (line)
  (gitgit-status--can-default line))

(defun gitgit-status--can-remove (line)
  (gitgit-status--can-default line))

(defun gitgit-status--can-auto-revert (line)
  (gitgit-status--can-default line))

(defun gitgit-status--can-diff (line)
  (gitgit-status--can-default line))

(defun gitgit-status--can-diff-staged (line)
  (cond
   ((string-match (concat "^\\([^ ].\\)" gitgit-status--file-name-regexp) line)
    (cons (match-string 1 line) (match-string 2 line)))
   (t nil)))

(defun gitgit-status--can-diff-not-staged (line)
  (cond
   ((string-match (concat "^\\( .\\)" gitgit-status--file-name-regexp) line)
    (cons (match-string 1 line) (match-string 2 line)))
   (t nil)))

(defun gitgit-status--can-log (line)
  (gitgit-status--can-default line))

(defun gitgit-status--can-rename (line)
  (gitgit-status--can-default line))

(provide 'gitgit-status-interactive)
