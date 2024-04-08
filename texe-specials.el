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

(defvar texe-special-default-hash (make-hash-table :test 'equal))
(defvar texe-special-default-alist (list (cons 'texe-special-call-texe-callback-p t)
                                         (cons 'texe-special-display-process-buffer-p
                                               t)
                                         (cons 'texe-special-display-process-running-p
                                               t)
                                         (cons 'texe-special-goto-point-max-p nil)
                                         (cons 'texe-special-goto-point-min-p nil)
                                         (cons 'texe-special-keep-select-texe-buffer-p
                                               nil)
                                         (cons 'texe-special-use-default-p t)
                                         (cons 'texe-special-append-shell-command nil)
                                         (cons 'texe-special-buffer-name-suffix nil)
                                         (cons 'texe-special-buffer-name-suffix-time
                                               nil)
                                         (cons 'texe-special-buffer-major-mode nil)
                                         (cons 'texe-special-user-callback nil)))
(defun texe-specials-setup ()
  (mapc (lambda (a)
          (puthash (car a)
                   (cdr a)
                   texe-special-default-hash))
        texe-special-default-alist))

;; setup
(texe-specials-setup)

(defun texe-special-set-call-texe-callback-p (texe-special-call-texe-callback-p)
  (puthash 'texe-special-call-texe-callback-p
           texe-special-call-texe-callback-p tmp-special-local-result-hash))

(defun texe-special-set-display-process-buffer-p (texe-special-display-process-buffer-p)
  (puthash 'texe-special-display-process-buffer-p
           texe-special-display-process-buffer-p tmp-special-local-result-hash))

(defun texe-special-set-display-process-running-p (texe-special-display-process-running-p)
  (puthash 'texe-special-display-process-running-p
           texe-special-display-process-running-p tmp-special-local-result-hash))

(defun texe-special-set-goto-point-min-p (texe-special-goto-point-min-p)
  (puthash 'texe-special-goto-point-min-p texe-special-goto-point-min-p
           tmp-special-local-result-hash))

(defun texe-special-set-goto-point-max-p (texe-special-goto-point-max-p)
  (puthash 'texe-special-goto-point-max-p texe-special-goto-point-max-p
           tmp-special-local-result-hash))

(defun texe-special-set-keep-select-texe-buffer-p (texe-special-keep-select-texe-buffer-p)
  (puthash 'texe-special-keep-select-texe-buffer-p
           texe-special-keep-select-texe-buffer-p tmp-special-local-result-hash))

(defun texe-special-set-use-default-p (texe-special-use-default-p)
  (puthash 'texe-special-use-default-p texe-special-use-default-p
           tmp-special-local-result-hash))

(defun texe-special-set-append-shell-command (texe-special-append-shell-command)
  (puthash 'texe-special-append-shell-command
           texe-special-append-shell-command tmp-special-local-result-hash))

(defun texe-special-set-buffer-name-suffix (texe-special-buffer-name-suffix)
  (puthash 'texe-special-buffer-name-suffix
           texe-special-buffer-name-suffix tmp-special-local-result-hash))

(defun texe-special-set-buffer-name-suffix-time (texe-special-buffer-name-suffix-time)
  (puthash 'texe-special-buffer-name-suffix-time
           texe-special-buffer-name-suffix-time tmp-special-local-result-hash))

(defun texe-special-set-major-mode (texe-special-buffer-major-mode)
  (puthash 'texe-special-buffer-major-mode texe-special-buffer-major-mode
           tmp-special-local-result-hash))

(defun texe-special-set-user-callback (texe-special-user-callback)
  (puthash 'texe-special-user-callback texe-special-user-callback
           tmp-special-local-result-hash))

(defun texe-special-rerun-p ()
  tmp-special-local-rerun-p)

(provide 'texe-specials)
