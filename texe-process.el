;;; texe-process.el -- Command Text Executor -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

;;; Code:

(defconst texe--special-comment-regexp-elisp
  "^#@elisp" "elisp")
(defconst texe--special-comment-regexp-script
  "^#@script" "script")
(defconst texe--default-tmp-directory "/tmp")
(defconst texe--process-mode-name " Texe Process")
(defconst texe--process-back-buffer-suffix
  "*ProcessBackBuffer*" "process 用 back buffer suffix 。")

(defvar texe-sentinel-callback-hook nil "texe 実行後に呼び出される callback
texe 外部から実行後のタイミングで呼び出したい場合に使用する。")

(define-minor-mode texe-mode-process-mode
  "Toggle Texe proces mode in the current buffer."
  :lighter texe--process-mode-name
  (if texe-mode-process-mode
      (use-local-map texe-mode-process-mode-map)
    (use-local-map nil)))

(defun texe-process-running-p (buffer-name)
  (gethash buffer-name texe-process-running-p-hash))

(defun texe-process-get-texe-buffer-name ()
  "texe-process から texe buffer name を取得する"
  (cdr (assq 'i-texe-buffer-name texe-process-local-args-alist)))

(defun texe--get-process-buffer-name (buffer-suffix)
  (concat (buffer-name)
          " "
          buffer-suffix))

(defun texe-get-process-back-buffer-name (buffer-name)
  (concat buffer-name " " texe--process-back-buffer-suffix))

(defun texe-get-process-buffer-name-from-back-buffer-name (back-buffer-name)
  (replace-regexp-in-string (concat " "
                                    (regexp-quote texe--process-back-buffer-suffix)
                                    "$")
                            ""
                            back-buffer-name))

(defun texe-run-core-special (special command buffer-suffix buffer-erase-p)
  (cond
   ((string-match texe--special-comment-regexp-elisp
                  special)
    (let ((texe-buffer-name (buffer-name)))
      (with-temp-buffer
        (insert (concat "(let ((texe-buffer-name \"" texe-buffer-name
                        "\"))"))
        (insert command)
        (insert ")")
        (eval-buffer))))
   ((string-match texe--special-comment-regexp-script
                  special)
    (let* ((script-tmpfile (make-temp-name (concat texe--default-tmp-directory "/texe_script")))
           (args-alist (list (cons 'script-tmpfile script-tmpfile))))
      (with-temp-file script-tmpfile
        (insert command))
      (set-file-modes script-tmpfile 448) ; 0700
      (texe-run-start-process nil
                              special
                              command
                              (texe--get-process-buffer-name buffer-suffix)
                              args-alist
                              (lambda ()
                                (texe--sentinel-callback)
                                (delete-file (cdr (assq 'script-tmpfile texe-process-local-args-alist))))
                              buffer-erase-p)))
   (t (let ((found (catch 'found
                     (mapc #'(lambda (regex-func)
                               (when (string-match (car regex-func) special)
                                 (funcall (cdr regex-func)
                                          special
                                          command)
                                 (throw 'found t)))
                           texe-mode-local-run-core-special-alist)
                     nil)))
        (unless found
          (texe-run-start-process nil
                                  special
                                  command
                                  (texe--get-process-buffer-name buffer-suffix)
                                  (list (cons 'i-from-texe t))
                                  'texe--sentinel-callback
                                  buffer-erase-p))))))

(defun texe--run-core (special command buffer-suffix buffer-erase-p)
  (if special
      (texe-run-core-special special command buffer-suffix
                             buffer-erase-p)
    (texe-run-start-process nil
                            special
                            command
                            (texe--get-process-buffer-name buffer-suffix)
                            (list (cons 'i-from-texe t))
                            'texe--sentinel-callback
                            buffer-erase-p)))

(defun texe--sentinel-callback ()
  "texe sentinel callback
texe 実行後に実行される callback 。"
  (run-hooks 'texe-sentinel-callback-hook)
  (texe-update-point)
  (texe-special-update-point texe-process-local-special-result)
  ;; ここで mode が変わると buffer local 変数が消えることに注意
  (texe-special-change-major-mode-if-match texe-process-local-special-result))

(defun texe-special-change-major-mode-if-match (special-result)
  (let ((process-major-mode (cdr (assq 'texe-special-set-major-mode special-result))))
    (when process-major-mode
      (let ((copy-local-variable-list (texe-process-get-local-variable-list)))
        (funcall process-major-mode)
        (texe-process-make-local-variable)
        (texe-process-update-local-variable-list copy-local-variable-list)))))

(defun texe-process-get-local-variable-list ()
  (if (boundp 'texe-process-local-backup-point-alist)
      (list texe-process-local-backup-point-alist
            texe-process-local-buffer-erase-p texe-process-local-special
            texe-process-local-special-result texe-process-local-command
            texe-process-local-process texe-process-local-args-alist
            texe-process-local-sentinel-callback texe-process-local-run-last-buffer-point
            texe-process-local-background-p)
    nil))

(defun texe-process-update-local-variable-list (variable-list)
  (when variable-list
    (setq texe-process-local-backup-point-alist (nth 0 variable-list))
    (setq texe-process-local-buffer-erase-p (nth 1 variable-list))
    (setq texe-process-local-special (nth 2 variable-list))
    (setq texe-process-local-special-result (nth 3 variable-list))
    (setq texe-process-local-command (nth 4 variable-list))
    (setq texe-process-local-process (nth 5 variable-list))
    (setq texe-process-local-args-alist (nth 6 variable-list))
    (setq texe-process-local-sentinel-callback (nth 7 variable-list))
    (setq texe-process-local-run-last-buffer-point (nth 8 variable-list))
    (setq texe-process-local-background-p (nth 9 variable-list))))

(defun texe-process-make-local-variable ()
  (set (make-local-variable 'texe-process-local-backup-point-alist)
       nil)
  (set (make-local-variable 'texe-process-local-buffer-erase-p)
       nil)
  (set (make-local-variable 'texe-process-local-special)
       nil)
  (set (make-local-variable 'texe-process-local-special-result)
       nil)
  (set (make-local-variable 'texe-process-local-command)
       nil)
  (set (make-local-variable 'texe-process-local-process)
       nil)
  (set (make-local-variable 'texe-process-local-args-alist)
       nil)
  (set (make-local-variable 'texe-process-local-sentinel-callback)
       nil)
  (set (make-local-variable 'texe-process-local-run-last-buffer-point)
       nil)
  (set (make-local-variable 'texe-process-local-background-p)
       nil))

(provide 'texe-process)
