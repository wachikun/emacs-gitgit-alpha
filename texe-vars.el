;;; texe-vars.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defface texe--face-special-comment '((((type x w32 mac ns)
                                        (class color)
                                        (background light))
                                       (:foreground "gray20" :bold t
                                                    :underline t))
                                      (((type x w32 mac ns)
                                        (class color)
                                        (background dark))
                                       (:foreground "gray30" :bold t
                                                    :underline t)))
  "special comment"
  :group 'texe-faces)

(defface texe--face-comment '((((type x w32 mac ns)
                                (class color)
                                (background light))
                               (:foreground "gray20" :bold t))
                              (((type x w32 mac ns)
                                (class color)
                                (background dark))
                               (:foreground "gray30" :bold t)))
  "comment"
  :group 'texe-faces)


;; minor-mode なので先頭の " " は必須であることに注意
(defconst texe--mode-name " Texe")

(defconst texe-special-comment-special-regexp
  "#@texe-default-special-regexp" "")

(defconst texe--special-comment-regexp-special-regexp (concat "^" texe-special-comment-special-regexp
                                                              "-")
  "")

(defvar texe-process-running-p-hash (make-hash-table :test 'equal))

(defvar texe-buffer-not-found-supplementary-message
  "" "texe buffer が見付からない場合の補足メッセージ")

(defvar texe-mode-map (make-sparse-keymap))
(define-key texe-mode-map "\C-c\C-c" 'texe-run)
(define-key texe-mode-map "\M-;" 'texe--comment)
(define-key texe-mode-map "\M-." 'texe--next-buffer)
(define-key texe-mode-map "\M-," 'texe--previous-buffer)

(defvar texe-mode-process-mode-map (make-sparse-keymap))
(define-key texe-mode-process-mode-map "\C-g"
            'texe-process-mode-cancel-process)
(define-key texe-mode-process-mode-map "\C-c\C-k"
            'texe-process-mode-cancel-process)
(define-key texe-mode-process-mode-map "!"
            'texe-process-mode-switch-to-texe)
(define-key texe-mode-process-mode-map "?"
            'texe-process-mode-show-last-command)
(define-key texe-mode-process-mode-map "g"
            'texe-rerun)
(define-key texe-mode-process-mode-map "\C-c\C-c"
            'texe-rerun)

(provide 'texe-vars)
