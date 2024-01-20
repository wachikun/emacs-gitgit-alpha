;;; texe-specials.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defun texe-special-buffer-name-suffix (suffix)
  (unless tmp-special-local-reload-p
    (add-to-list 'tmp-special-local-result-alist
                 (cons 'texe-special-buffer-name-suffix suffix))))

(defun texe-special-buffer-name-suffix-time (suffix)
  (unless tmp-special-local-reload-p
    (add-to-list 'tmp-special-local-result-alist
                 (cons 'texe-special-buffer-name-suffix-time
                       suffix))))

(defun texe-special-set-major-mode (major-mode-symbol)
  (add-to-list 'tmp-special-local-result-alist
               (cons 'texe-special-set-major-mode major-mode-symbol)))

(defun texe-special-keep-select-texe-buffer ()
  (add-to-list 'tmp-special-local-result-alist
               (list 'texe-special-keep-select-texe-buffer)))

(defun texe-special-no-display-process-buffer ()
  (add-to-list 'tmp-special-local-result-alist
               (list 'texe-special-no-display-process-buffer)))

(defun texe-special-ignore-process-running ()
  (add-to-list 'tmp-special-local-result-alist
               (list 'texe-special-ignore-process-running)))

(defun texe-special-set-point-min ()
  (add-to-list 'tmp-special-local-result-alist
               (list 'texe-special-set-point-min)))

(defun texe-special-set-point-max ()
  (add-to-list 'tmp-special-local-result-alist
               (list 'texe-special-set-point-max)))

(provide 'texe-specials)
