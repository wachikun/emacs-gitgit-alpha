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
  "point-alist から window-start のみを復帰する
window-start は大きく point がずれていると意図しない point 位置になる場合があるので、大きく
buffer が変化するような場合には復帰しない方が良いことに注意。"
  (let ((window-information-list (cdr (assq 'window-information-list point-alist))))
    (mapcar #'(lambda (window-information)
                (set-window-start (nth 0 window-information)
                                  (nth 2 window-information)))
            window-information-list)))

(defun texe-update-point ()
  "point と window-point を復帰する
texe-process-local-backup-point-alist が non-nil ならばそれで point を復帰し、
texe-process-local-backup-point-alist が nil ならば (point-min) に移動する。"
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
  "関数 texe-get-point-alist で取得した point-alist で point と window-point を復帰する
通常は excursion 系関数で復帰すべきだが、 erase-buffer などでバッファクリアして point が復帰
できないような場合に使用する。"
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
      ;; point-alist 取得時に window が存在した場合は window-information-list から復帰
      (mapc #'(lambda (window-information)
                (when (eq (window-buffer (nth 0 window-information)) (nth 3 window-information))
                  (add-to-list 'window-set-alist
                               (cons (nth 0 window-information) t))
                  (set-window-point (nth 0 window-information)
                                    (texe--get-line-beginning-position (nth 1 window-information)))))
            window-information-list)
      ;; point-alist 取得時に window が存在しなかった(後から window を表示したような)場合は buffer point で復帰
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
  "window-infomation-list を返す
window-infomation-list は下記のような構造。
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
