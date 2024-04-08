;;; texe-point.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defun texe-get-point-alist ()
  (let ((line (texe-get-line))
        (point (point))
        (window-information-list (texe-l-get-window-information-list (current-buffer))))
    (list (cons 'buffer (current-buffer))
          (cons 'line line)
          (cons 'point point)
          (cons 'window-information-list window-information-list))))

(defun texe-update-window-start (point-alist)
  "point-alist $B$+$i(B window-start $B$N$_$rI|5"$9$k(B
window-start $B$OBg$-$/(B point $B$,$:$l$F$$$k$H0U?^$7$J$$(B point $B0LCV$K$J$k>l9g$,$"$k$N$G!"Bg$-$/(B
buffer $B$,JQ2=$9$k$h$&$J>l9g$K$OI|5"$7$J$$J}$,NI$$$3$H$KCm0U!#(B"
  (let ((window-information-list (cdr (assq 'window-information-list point-alist))))
    (mapcar #'(lambda (window-information)
                (set-window-start (nth 0 window-information)
                                  (nth 2 window-information)))
            window-information-list)))

(defun texe-update-point ()
  "point $B$H(B window-point $B$rI|5"$9$k(B
texe-process-local-backup-point-alist $B$,(B non-nil $B$J$i$P$=$l$G(B point $B$rI|5"$7!"(B
texe-process-local-backup-point-alist $B$,(B nil $B$J$i$P(B (point-min) $B$K0\F0$9$k!#(B"
  (if texe-process-local-backup-point-alist
      (texe--update-point-alist texe-process-local-backup-point-alist)
    (goto-char (point-min))
    (mapcar #'(lambda (window)
                (set-window-start window
                                  (point-min))
                (set-window-point window
                                  (point-min)))
            (get-buffer-window-list (current-buffer)))))

(defun texe-special-update-point (special-result)
  (let ((point (cond
                ((gethash 'texe-special-goto-point-min-p special-result)
                 (point-min))
                ((gethash 'texe-special-goto-point-max-p special-result)
                 (point-max))
                (t nil))))
    (when point
      (goto-char point)
      (mapcar #'(lambda (window)
                  (set-window-start window
                                    (texe-l-get-window-start-point window point))
                  (set-window-point window point))
              (get-buffer-window-list (current-buffer))))))

(defun texe-set-point-min ()
  (goto-char (point-min))
  (mapcar #'(lambda (window)
              (set-window-start window
                                (point))
              (set-window-point window
                                (point)))
          (get-buffer-window-list (current-buffer))))

(defun texe--get-line-beginning-position (point)
  (save-excursion
    (goto-char point)
    (line-beginning-position)))

(defun texe--update-point-alist (point-alist)
  "$B4X?t(B texe-get-point-alist $B$G<hF@$7$?(B point-alist $B$G(B point $B$H(B window-point $B$rI|5"$9$k(B
$BDL>o$O(B excursion $B7O4X?t$GI|5"$9$Y$-$@$,!"(B erase-buffer $B$J$I$G%P%C%U%!%/%j%"$7$F(B point $B$,I|5"(B
$B$G$-$J$$$h$&$J>l9g$K;HMQ$9$k!#(B"
  (let (line-found-point (line (cdr (assq 'line point-alist)))
                         (point (cdr (assq 'point point-alist)))
                         (buffer (cdr (assq 'buffer point-alist)))
                         (window-information-list (cdr (assq 'window-information-list point-alist))))
    (unless (eq (current-buffer) buffer)
      (error "illegal buffer current-buffer=%s  alist-buffer=%s"
             (buffer-name)
             (buffer-name buffer)))
    (when line
      (let ((line-regex (concat "^"
                                (regexp-quote (cdr (assq 'line point-alist)))
                                "$")))
        (save-excursion
          (goto-char (point-min))
          (let ((tmp-found-point (re-search-forward line-regex nil t)))
            (unless (re-search-forward line-regex nil t)
              (setq line-found-point tmp-found-point))))))
    (goto-char (if line-found-point line-found-point point))
    (beginning-of-line)
    (let ((current-buffer-point (point)) window-set-alist)
      ;; point-alist $B<hF@;~$K(B window $B$,B8:_$7$?>l9g$O(B window-information-list $B$+$iI|5"(B
      (mapc #'(lambda (window-information)
                (when (eq (window-buffer (nth 0 window-information)) (nth 3 window-information))
                  (add-to-list 'window-set-alist
                               (cons (nth 0 window-information) t))
                  (set-window-point (nth 0 window-information)
                                    (texe--get-line-beginning-position (nth 1 window-information)))))
            window-information-list)
      ;; point-alist $B<hF@;~$K(B window $B$,B8:_$7$J$+$C$?(B($B8e$+$i(B window $B$rI=<($7$?$h$&$J(B)$B>l9g$O(B buffer point $B$GI|5"(B
      (mapcar #'(lambda (window)
                  (unless (assq window window-set-alist)
                    (set-window-point window current-buffer-point)))
              (get-buffer-window-list (current-buffer))))))

(defun texe-l-get-window-start-point (window point)
  (with-current-buffer (window-buffer window)
    (save-excursion
      (goto-char point)
      (beginning-of-line)
      (forward-line (- (- (window-body-height window)
                          1)))
      (point))))

(defun texe-l-get-window-information-list (buffer)
  "window-infomation-list $B$rJV$9(B
window-infomation-list $B$O2<5-$N$h$&$J9=B$!#(B
\='((window window-point0 window-start0 buffer-name0)
  (window1 window-point1 window-start1 buffer-name1)...)"
  (let (result)
    (mapc #'(lambda (window)
              (setq result (append result
                                   (list (list window
                                               (window-point window)
                                               (window-start window)
                                               buffer)))))
          (get-buffer-window-list buffer))
    result))

(provide 'texe-point)
