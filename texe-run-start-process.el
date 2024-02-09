;;; texe-run-start-process.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defface texe--face-process-running-buffer-content '((((type x w32 mac ns)
                                                       (class color)
                                                       (background light))
                                                      (:foreground "gray50"))
                                                     (((type x w32 mac ns)
                                                       (class color)
                                                       (background dark))
                                                      (:foreground "gray50")))
  "process running buffer content"
  :group 'texe-faces)

(defface texe--face-process-running-header-line '((((type x w32 mac ns)
                                                    (class color)
                                                    (background light))
                                                   (:foreground "gray20" :height 200))
                                                  (((type x w32 mac ns)
                                                    (class color)
                                                    (background dark))
                                                   (:foreground "gray30" :height 200)))
  "process running header line"
  :group 'texe-faces)

(defconst texe--process-running-message-delay-second
  1)

(defconst texe--process-running-recenter-delay-second-first
  0.1)

(defconst texe--process-running-recenter-delay-second-second
  2)

(defvar texe--processes 0)

(defun texe-run-start-process (background-p special command async-process-buffer-name
                                            args-alist &optional sentinel-callback buffer-erase-p
                                            reload-p)
  (setq special (texe--apply-special-from-default-special-regexp-list-if-needed
                 special command))
  (let ((special-result (texe--eval-special special reload-p)))
    (setq async-process-buffer-name (texe--modify-async-process-buffer-name-by-special
                                     special-result async-process-buffer-name))
    (let* ((call-texe-buffer-name (buffer-name)) backup-point-alist
           (async-process-back-buffer-name (texe-get-process-back-buffer-name async-process-buffer-name))
           (current-async-process-buffer-name (if background-p async-process-back-buffer-name
                                                async-process-buffer-name)))
      (if (get-process current-async-process-buffer-name)
          (message "process running")
        (setq backup-point-alist (texe--setup-async-process-buffer async-process-buffer-name
                                                                   buffer-erase-p args-alist))
        (when (and (not (cdr (assq 'no-display-process-buffer args-alist)))
                   (not (assq 'texe-special-ignore-process-running
                              special-result)))
          (if background-p
              (texe--setup-background-run-at-time async-process-buffer-name
                                                  async-process-back-buffer-name)
            (texe--setup-foreground-run-at-time async-process-buffer-name)))
        (texe--setup-process-buffer background-p special-result
                                    special command args-alist sentinel-callback
                                    call-texe-buffer-name backup-point-alist current-async-process-buffer-name)
        (setq texe--processes (1+ texe--processes))
        (let ((run-last-buffer-point (point)))
          (when (assq 'i-from-texe args-alist)
            (with-current-buffer (get-buffer-create current-async-process-buffer-name)
              (texe-set-header-line-process-start)
              (setq texe-process-local-run-last-buffer-point
                    run-last-buffer-point)
              (when background-p
                (let ((copy-process texe-process-local-process))
                  (with-current-buffer (get-buffer-create async-process-buffer-name)
                    (setq texe-process-local-process copy-process)))))))
        (puthash current-async-process-buffer-name
                 t texe-process-running-p-hash)
        (texe--display-async-process-buffer background-p
                                            special-result async-process-buffer-name async-process-back-buffer-name
                                            args-alist)))))

(defun texe-set-header-line-process-runnning ()
  (setq header-line-format (propertize "PROCESS RUNNING" 'face 'texe--face-process-running-header-line)))

(defun texe-set-header-line-process-terminated ()
  (setq header-line-format (propertize "TERMINATED" 'face 'texe--face-process-running-header-line)))

(defun texe-set-header-line-process-start ()
  (setq header-line-format nil))

(defun texe-set-header-line-process-success ()
  (setq header-line-format nil))

(defun texe-set-header-line-process-error (event)
  (setq header-line-format (propertize (format "PROCESS ERROR event = %s"
                                               (replace-regexp-in-string "[\r\n]+$" "" event))
                                       'face
                                       'texe--face-process-running-header-line)))

(defun texe--apply-special-from-default-special-regexp-list-if-needed (special command)
  (let ((special-force-yes-only-p (and (stringp special)
                                       (string-match "^ *#@FORCE-YES *$" special))))
    (if (or (not special)
            special-force-yes-only-p)
        (let (result)
          (catch 'mapcar
            (mapcar (lambda (pair)
                      (let ((default-special (nth 0 pair))
                            (command-regexp (nth 1 pair)))
                        (when (string-match command-regexp command)
                          (setq result default-special)
                          (throw 'mapcar nil))))
                    (seq-partition texe-mode-local-default-special-regexp-list
                                   2)))
          (if special-force-yes-only-p
              (concat "FORCE-YES " result)
            result))
      special)))

(defun texe--modify-async-process-buffer-name-by-special (special-result async-process-buffer-name)
  (cond
   ((assq 'texe-special-buffer-name-suffix special-result)
    (setq async-process-buffer-name (concat async-process-buffer-name
                                            (replace-regexp-in-string " "
                                                                      "-"
                                                                      (cdr (assq 'texe-special-buffer-name-suffix special-result))))))
   ((assq 'texe-special-buffer-name-suffix-time
          special-result)
    (setq async-process-buffer-name (concat async-process-buffer-name
                                            (replace-regexp-in-string " "
                                                                      "-"
                                                                      (cdr (assq 'texe-special-buffer-name-suffix-time
                                                                                 special-result)))
                                            "-"
                                            (format-time-string "%Y-%m-%d-%H:%M:%S"))))
   (t async-process-buffer-name)))

(defun texe--setup-async-process-buffer (async-process-buffer-name buffer-erase-p
                                                                   args-alist)
  (let (result-backup-point-alist)
    (when (get-buffer async-process-buffer-name)
      (with-current-buffer async-process-buffer-name
        (when buffer-erase-p
          (setq result-backup-point-alist (texe-get-point-alist))
          (setq buffer-read-only nil)
          (erase-buffer)
          (setq buffer-read-only t))))
    (with-current-buffer (get-buffer-create async-process-buffer-name)
      (setq buffer-undo-list t)
      (setq buffer-read-only nil)
      (when (assq 'i-from-texe args-alist)
        (texe-mode-process-mode)
        (texe-process-make-local-variable))
      (setq buffer-read-only t))
    result-backup-point-alist))

(defun texe--setup-background-run-at-time (async-process-buffer-name async-process-back-buffer-name)
  (run-at-time texe--process-running-message-delay-second
               nil
               (lambda ()
                 (when (and (get-buffer async-process-buffer-name)
                            (get-buffer async-process-back-buffer-name))
                   (let ((window (get-buffer-window async-process-buffer-name)))
                     (if window
                         (set-window-buffer window async-process-back-buffer-name)
                       (switch-to-buffer async-process-back-buffer-name)))
                   (when (get-process async-process-back-buffer-name)
                     (with-current-buffer async-process-back-buffer-name
                       (texe-set-header-line-process-runnning)))))))

(defun texe--setup-foreground-run-at-time (async-process-buffer-name)
  (run-at-time texe--process-running-message-delay-second
               nil
               (lambda ()
                 (when (and (get-buffer async-process-buffer-name)
                            (get-process async-process-buffer-name))
                   (with-current-buffer async-process-buffer-name
                     (texe-set-header-line-process-runnning)
                     (texe--show-process-buffer-content (current-buffer))))))
  (run-at-time texe--process-running-recenter-delay-second-first
               nil
               (lambda ()
                 (when (and (get-buffer async-process-buffer-name)
                            (get-process async-process-buffer-name))
                   (with-current-buffer async-process-buffer-name
                     (texe--show-process-buffer-content (current-buffer))))))
  (run-at-time texe--process-running-recenter-delay-second-second
               nil
               (lambda ()
                 (when (and (get-buffer async-process-buffer-name)
                            (get-process async-process-buffer-name))
                   (with-current-buffer async-process-buffer-name
                     (texe--show-process-buffer-content (current-buffer)))))))

(defun texe--setup-process-buffer (background-p special-result special command
                                                args-alist sentinel-callback call-texe-buffer-name
                                                backup-point-alist current-async-process-buffer-name)
  (with-current-buffer (get-buffer-create current-async-process-buffer-name)
    (setq buffer-undo-list t)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq buffer-read-only t)
    (let ((command-append (cdr (assq 'texe-special-append-shell-command special-result))))
      (when command-append
        (setq command (concat command command-append))))
    (with-environment-variables (("PAGER" ""))
      (let* ((script-tmpfile (cdr (assq 'script-tmpfile args-alist)))
             (process (start-process-shell-command current-async-process-buffer-name
                                                   current-async-process-buffer-name
                                                   (if script-tmpfile script-tmpfile command))))
        (set-process-sentinel process 'texe--process-sentinel)
        (texe-mode-process-mode)
        (texe-process-make-local-variable)
        (setq texe-process-local-backup-point-alist
              backup-point-alist)
        (setq texe-process-local-special special)
        (setq texe-process-local-special-result special-result)
        (setq texe-process-local-command command)
        (setq texe-process-local-process process)
        (setq texe-process-local-args-alist args-alist)
        (setq texe-process-local-sentinel-callback
              sentinel-callback)
        (setq texe-process-local-background-p background-p)))
    (add-to-list 'texe-process-local-args-alist
                 (cons 'i-texe-buffer-name call-texe-buffer-name))))

(defun texe--display-async-process-buffer (background-p special-result async-process-buffer-name
                                                        async-process-back-buffer-name args-alist)
  (if background-p
      (let ((back-buffer-window (get-buffer-window async-process-back-buffer-name)))
        (unless (cdr (assq 'no-display-process-buffer args-alist))
          (cond
           ((assq 'texe-special-no-display-process-buffer
                  special-result))
           ((assq 'texe-special-keep-select-texe-buffer
                  special-result)
            (if back-buffer-window
                (set-window-buffer back-buffer-window async-process-buffer-name)
              (display-buffer async-process-buffer-name)))
           (t (if back-buffer-window
                  (set-window-buffer back-buffer-window async-process-buffer-name)
                (pop-to-buffer async-process-buffer-name))))))
    (unless (cdr (assq 'no-display-process-buffer args-alist))
      (cond
       ((assq 'texe-special-no-display-process-buffer
              special-result))
       ((assq 'texe-special-keep-select-texe-buffer
              special-result)
        (display-buffer async-process-buffer-name))
       (t (pop-to-buffer async-process-buffer-name))))))

(defun texe--eval-special (special reload-p)
  (if special
      (with-temp-buffer
        (set (make-local-variable 'tmp-special-local-result-alist)
             nil)
        (set (make-local-variable 'tmp-special-local-reload-p)
             reload-p)
        (when (string-match "^\#?\@?[^(]*\\((.+\\)" special)
          (eval (car (read-from-string (match-string 1 special)))))
        tmp-special-local-result-alist)
    nil))

(defun texe--process-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (remhash (buffer-name)
             texe-process-running-p-hash)
    (setq texe--processes (1- texe--processes))
    (let* ((source-buffer-name (buffer-name))
           (source-min (point-min))
           (source-max (point-max))
           (destination-buffer-name (texe-get-process-buffer-name-from-back-buffer-name (buffer-name)))
           (backup-point-alist (with-current-buffer destination-buffer-name
                                 (texe-get-point-alist)))
           (copy-local-variable-list (texe-process-get-local-variable-list)))
      (if (and texe-process-local-background-p
               (get-buffer destination-buffer-name))
          (with-current-buffer destination-buffer-name
            (setq buffer-read-only nil)
            (if (texe--use-replace-buffer-contents-p source-max)
                (replace-buffer-contents source-buffer-name
                                         1 1000)
              (erase-buffer)
              (insert-buffer-substring source-buffer-name
                                       source-min source-max))
            (setq buffer-read-only t)
            (kill-buffer source-buffer-name)
            (texe-process-make-local-variable)
            (texe-process-update-local-variable-list copy-local-variable-list)
            (when backup-point-alist
              (setq texe-process-local-backup-point-alist
                    backup-point-alist))
            (when texe-process-local-sentinel-callback
              (funcall texe-process-local-sentinel-callback)))
        (texe--show-process-buffer-content (current-buffer))
        (when texe-process-local-sentinel-callback
          (funcall texe-process-local-sentinel-callback))))
    (cond
     ((string-match "^finished" event)
      (unless texe-process-local-donot-touch-header-on-success
        (texe-set-header-line-process-success))
      (when texe-process-local-buffer-kill-p
        (kill-buffer)))
     ((string-match "^exited abnormally with code"
                    event)
      (display-buffer (buffer-name))
      (texe-set-header-line-process-error event))
     (t (message (concat "process error error:" event))))))

(defun texe--show-process-buffer-content (buffer-name)
  (let ((window (get-buffer-window buffer-name)))
    (when window
      (with-selected-window window
        (recenter)))))

(provide 'texe-run-start-process)
