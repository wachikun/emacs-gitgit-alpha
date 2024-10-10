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

(defconst texe--header-line-process-time-long-run-message-second
  5)

(defconst texe--special-eval-lisp-code-regexp
  "^\#?\@?[^(]*\\((.+\\)")

(defconst texe--special-prefix-regexp "^texe-special-")

(defvar texe--processes 0)

(defun texe-run-start-process (background-p special command async-process-buffer-name
                                            args-alist &optional sentinel-callback buffer-erase-p
                                            rerun-p force-yes-p)
  (unless rerun-p
    (setq special (texe-l-apply-special-from-default-special-regexp-list-if-needed
                   special command)))
  (let ((special-result (texe-l-eval-special special rerun-p)))
    (unless rerun-p
      (setq async-process-buffer-name (texe-l-modify-async-process-buffer-name-by-special
                                       special-result async-process-buffer-name)))
    (let* ((call-texe-buffer-name (buffer-name)) backup-point-alist
           (async-process-back-buffer-name (texe-get-process-back-buffer-name async-process-buffer-name))
           (current-async-process-buffer-name (if background-p async-process-back-buffer-name
                                                async-process-buffer-name)))
      (if (get-process current-async-process-buffer-name)
          (message "process running")
        (setq backup-point-alist (texe-l-setup-async-process-buffer async-process-buffer-name
                                                                    buffer-erase-p args-alist))
        (when (and (not (cdr (assq 'no-display-process-buffer args-alist)))
                   (gethash 'texe-special-display-process-running-p
                            special-result))
          (if background-p
              (texe-l-setup-background-run-at-time async-process-buffer-name
                                                   async-process-back-buffer-name)
            (texe-l-setup-foreground-run-at-time async-process-buffer-name)))
        (texe-l-setup-process-buffer background-p
                                     special-result special command args-alist
                                     sentinel-callback rerun-p force-yes-p call-texe-buffer-name
                                     backup-point-alist current-async-process-buffer-name)
        (setq texe--processes (1+ texe--processes))
        (with-current-buffer (get-buffer current-async-process-buffer-name)
          (texe-set-header-line-process-start))
        (when background-p
          (with-current-buffer (get-buffer async-process-buffer-name)
            (texe-set-header-line-process-start)))
        (let ((run-last-buffer-point (point)))
          (when (assq 'i-from-texe args-alist)
            (with-current-buffer (get-buffer-create current-async-process-buffer-name)
              (setq texe-process-local-run-last-buffer-point
                    run-last-buffer-point)
              (when background-p
                (let ((copy-process texe-process-local-process))
                  (with-current-buffer (get-buffer-create async-process-buffer-name)
                    (setq texe-process-local-process copy-process)))))))
        (puthash current-async-process-buffer-name
                 t texe-process-running-p-hash)
        (texe-l-display-async-process-buffer background-p
                                             special-result async-process-buffer-name async-process-back-buffer-name
                                             args-alist)))))

(defun texe-set-header-line-process-terminated ()
  (texe-set-header-line-process-time "TERMINATED"))

(defun texe-set-header-line-process-start ()
  (setq header-line-format (concat "PROCESS START "
                                   (format-time-string "%Y-%m-%d %H:%M:%S"))))

(defun texe-set-header-line-process-success ()
  (texe-set-header-line-process-time "DONE"))

(defun texe-set-header-line-process-error (event)
  (texe-set-header-line-process-time (concat "PROCESS ERROR event = "
                                             (replace-regexp-in-string "[\r\n]+$" "" event))))

(defun texe-set-header-line-process-time (base)
  (let* ((diff-sec (float-time (time-subtract (current-time)
                                              (cdr (assq 'start-time texe-process-local-information)))))
         (message (format "%s %s - %s"
                          (format "%s %s"
                                  base
                                  (texe-sec-to-hms diff-sec))
                          (format-time-string "%Y-%m-%d %H:%M:%S"
                                              (cdr (assq 'start-time texe-process-local-information)))
                          (format-time-string "%Y-%m-%d %H:%M:%S"))))
    (setq header-line-format message)
    (when (and (cdr (assq 'long-run-message-p texe-process-local-information))
               (>= diff-sec texe--header-line-process-time-long-run-message-second))
      (let ((inhibit-message t))
        (message "%s %s"
                 (buffer-name)
                 message)))))

(defun texe-sec-to-hms (sec)
  (let ((tmp ""))
    (when (>= sec 3600)
      (let ((hour (truncate (/ sec 3600))))
        (setq sec (- sec
                     (* hour 3600)))
        (setq tmp (format "%d hour " hour))))
    (when (>= sec 60)
      (let ((min (truncate (/ sec 60))))
        (setq sec (- sec
                     (* min 60)))
        (setq tmp (format "%s%d min. " tmp min))))
    (format "%s%f sec." tmp sec)))

(defun texe-l-apply-special-from-default-special-regexp-list-if-needed (special command)
  (let ((special-result (texe-l-eval-special special nil)))
    (if (gethash 'texe-special-use-default-p special-result)
        (let ((found-default-special ""))
          (catch 'mapcar
            (mapcar #'(lambda (pair)
                        (let ((default-special (nth 0 pair))
                              (command-regexp (nth 1 pair)))
                          (when (string-match command-regexp command)
                            (setq found-default-special default-special)
                            (throw 'mapcar nil))))
                    (seq-partition texe-mode-local-default-special-regexp-list
                                   2)))
          (if (stringp special)
              ;; S $B<0$,$"$k(B
              (cond
               ((string-match "^\\(.+?\\)\\((.+\\)$" special)
                (concat (match-string 1 special)
                        found-default-special
                        (match-string 2 special)))
               ;; S $B<0$,$J$$(B
               ((string-match "^\\([^(]+\\)$" special)
                (concat (match-string 1 special)
                        found-default-special))
               (t (concat found-default-special special)))
            (concat found-default-special special)))
      special)))

(defun texe-l-modify-async-process-buffer-name-by-special (special-result async-process-buffer-name)
  (cond
   ((gethash 'texe-special-buffer-name-suffix
             special-result)
    (setq async-process-buffer-name (concat async-process-buffer-name
                                            (replace-regexp-in-string " "
                                                                      "-"
                                                                      (gethash 'texe-special-buffer-name-suffix
                                                                               special-result)))))
   ((gethash 'texe-special-buffer-name-suffix-time
             special-result)
    (setq async-process-buffer-name (concat async-process-buffer-name
                                            (replace-regexp-in-string " "
                                                                      "-"
                                                                      (gethash 'texe-special-buffer-name-suffix-time
                                                                               special-result))
                                            "-"
                                            (format-time-string "%Y-%m-%d-%H:%M:%S"))))
   (t async-process-buffer-name)))

(defun texe-l-setup-async-process-buffer (async-process-buffer-name buffer-erase-p
                                                                    args-alist)
  (let (result-backup-point-alist)
    (when (get-buffer async-process-buffer-name)
      (with-current-buffer async-process-buffer-name
        (setq result-backup-point-alist (texe-get-point-alist))
        (when buffer-erase-p
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

(defun texe-l-setup-background-run-at-time (async-process-buffer-name async-process-back-buffer-name)
  (run-at-time texe--process-running-message-delay-second
               nil
               #'(lambda ()
                   (when (and (get-buffer async-process-buffer-name)
                              (get-buffer async-process-back-buffer-name))
                     (let ((window (get-buffer-window async-process-buffer-name)))
                       (if window
                           (set-window-buffer window async-process-back-buffer-name)
                         (switch-to-buffer async-process-back-buffer-name)))))))

(defun texe-l-setup-foreground-run-at-time (async-process-buffer-name)
  (texe-l-setup-foreground-run-at-time-1 async-process-buffer-name
                                         texe--process-running-message-delay-second)
  (texe-l-setup-foreground-run-at-time-1 async-process-buffer-name
                                         texe--process-running-recenter-delay-second-first)
  (texe-l-setup-foreground-run-at-time-1 async-process-buffer-name
                                         texe--process-running-recenter-delay-second-second))

(defun texe-l-setup-foreground-run-at-time-1 (async-process-buffer-name delay-second)
  (run-at-time delay-second
               nil
               #'(lambda ()
                   (when (and (get-buffer async-process-buffer-name)
                              (get-process async-process-buffer-name))
                     (with-current-buffer async-process-buffer-name
                       (texe-l-show-process-buffer-content (current-buffer)))))))

(defun texe-l-setup-process-buffer (background-p special-result special command
                                                 args-alist sentinel-callback rerun-p force-yes-p
                                                 call-texe-buffer-name backup-point-alist current-async-process-buffer-name)
  (let ((caller-default-directory default-directory))
    (with-current-buffer (get-buffer-create current-async-process-buffer-name)
      (setq default-directory caller-default-directory)
      (setq buffer-undo-list t)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq buffer-read-only t)
      (with-environment-variables (("PAGER" ""))
        (let* ((script-tmpfile (cdr (assq 'script-tmpfile args-alist))) process)
          (unless (or rerun-p script-tmpfile)
            (let ((command-append (gethash 'texe-special-append-shell-command
                                           special-result)))
              (when command-append
                (setq command (concat command command-append)))))
          (setq process (start-process-shell-command (buffer-name)
                                                     (buffer-name)
                                                     (if script-tmpfile script-tmpfile command)))
          (set-process-sentinel process 'texe-l-process-sentinel)
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
          (setq texe-process-local-information (list (cons 'buffer-name (buffer-name))
                                                     (cons 'special special)
                                                     (cons 'command command)
                                                     (cons 'force-yes-p force-yes-p)
                                                     (cons 'start-time (current-time))
                                                     (cons 'long-run-message-p t)))
          (setq texe-process-local-background-p background-p)))
      (add-to-list 'texe-process-local-args-alist
                   (cons 'i-texe-buffer-name call-texe-buffer-name)))))

(defun texe-l-display-async-process-buffer (background-p special-result async-process-buffer-name
                                                         async-process-back-buffer-name args-alist)
  (if background-p
      (let ((back-buffer-window (get-buffer-window async-process-back-buffer-name)))
        (unless (cdr (assq 'no-display-process-buffer args-alist))
          (cond
           ((not (gethash 'texe-special-display-process-buffer-p
                          special-result))
            (if back-buffer-window
                (set-window-buffer back-buffer-window async-process-buffer-name)
              (pop-to-buffer async-process-buffer-name)))
           ((gethash 'texe-special-keep-select-texe-buffer-p
                     special-result)
            (if back-buffer-window
                (set-window-buffer back-buffer-window async-process-buffer-name)
              (display-buffer async-process-buffer-name)))
           (t (if back-buffer-window
                  (set-window-buffer back-buffer-window async-process-buffer-name)
                (pop-to-buffer async-process-buffer-name))))))
    (unless (cdr (assq 'no-display-process-buffer args-alist))
      (cond
       ((not (gethash 'texe-special-display-process-buffer-p
                      special-result))
        (pop-to-buffer async-process-buffer-name))
       ((gethash 'texe-special-keep-select-texe-buffer-p
                 special-result)
        (display-buffer async-process-buffer-name))
       (t (pop-to-buffer async-process-buffer-name))))))

(defun texe-l-eval-special (special rerun-p)
  (catch 'error
    (if special
        (with-temp-buffer
          (set (make-local-variable 'tmp-special-local-result-hash)
               (copy-hash-table texe-special-default-hash))
          (set (make-local-variable 'tmp-special-local-rerun-p)
               rerun-p)
          (when (string-match texe--special-eval-lisp-code-regexp
                              special)
            (let* ((start 0)
                   (text (match-string 1 special))
                   (text-length (length text)))
              (while (< start text-length)
                (unless (ignore-errors (let ((read-result (read-from-string text start)))
                                         (eval (car read-result))
                                         (setq start (cdr read-result))))
                  (message "eval error text = %s" text)
                  (message "           read-from-string = %s"
                           (read-from-string text start))
                  (throw 'error tmp-special-local-result-hash)))))
          tmp-special-local-result-hash)
      (copy-hash-table texe-special-default-hash))))

(defun texe-l-process-sentinel (process event)
  (with-current-buffer (process-buffer process)
    (remhash (buffer-name)
             texe-process-running-p-hash)
    (setq texe--processes (1- texe--processes))
    (let ((destination-buffer-name (texe-get-process-buffer-name-from-back-buffer-name (buffer-name))))
      (if (and texe-process-local-background-p
               (get-buffer destination-buffer-name))
          (texe-l-process-sentinel-copy-background destination-buffer-name)
        (texe-l-show-process-buffer-content (current-buffer))
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

(defun texe-l-process-sentinel-copy-background (destination-buffer-name)
  (let ((source-buffer-name (buffer-name))
        (source-min (point-min))
        (source-max (point-max))
        (copy-local-variable-list (texe-process-get-local-variable-list))
        (backup-point-alist (with-current-buffer destination-buffer-name
                              (texe-get-point-alist))))
    (with-current-buffer destination-buffer-name
      (setq buffer-read-only nil)
      (if (texe-l-use-replace-buffer-contents-p source-max)
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
        (funcall texe-process-local-sentinel-callback)))))

(defun texe-l-show-process-buffer-content (buffer-name)
  (let ((window (get-buffer-window buffer-name)))
    (when window
      (with-selected-window window
        (recenter)))))

(defun texe-l-use-replace-buffer-contents-p (source-max)
  "replace-buffer-contents $B$OHs>o$KCY$$$?$a!"(B
$B;HMQ$9$k$N$O2<5->r7o$rK~$?$7$?>l9g$N$_(B

- window $B$,HsI=<((B
- source buffer $B$H(B destination buffer $B$N:9$,>.$5$$(B

$B!V(Bsource buffer $B$H(B destination buffer $B$N:9$,>.$5$$!W$H$O!"(B
source buffer $B$H(B destination buffer $B$N(B (point-max) $B$N:9$,(B
point-max-diff-threshold $B$h$j>.$5$$$H$$$&$3$H$r0UL#$9$k!#(B

point $B$d(B window-point $B$rI|5"$7$F$$$k$K$b4X$o$i$:(B replace-buffer-contents $B$,(B
$BI,MW$H$J$k$N$O!"(B
$B!V(Berase-buffer $B$J$I$G(B point $B$r<:$J$C$?HsI=<(%P%C%U%!$,D>8e$KI=<($5$l$k!W(B
$B$H$$$&FC<l$J%1!<%9$N$_$G!"B>$NA`:n$r64$s$G$+$iHsI=<(%P%C%U%!$rI=<($9$k$H(B
$BLdBj$J$/I|5"$G$-$k$?$a(B Emacs $B$N%P%0$+$b$7$l$J$$!#(B

$BK\Mh$O(B replace-buffer-contents $B$N0z?t$G$"$kDxEY@)8f$G$-$k$O$:$@$,!"(B
$B<c43F0:n$,2x$7$$>e!"$"$-$i$+$KBT$A;~4V$,H/@8$9$k$?$a!"(B
point-max-diff-threshold $B$K$h$j%P%C%U%!$N:9J,$r3NG'$7$F$$$k!#(B"
  (let ((point-max-diff-threshold 1000))
    (and (>= emacs-major-version 26)
         (not (get-buffer-window (current-buffer)))
         (< (abs (- source-max
                    (point-max))) point-max-diff-threshold))))

(provide 'texe-run-start-process)
