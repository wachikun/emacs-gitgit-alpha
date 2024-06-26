;;; gitgit-diff.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defconst gitgit-grep-mode-name "gitgit grep")

(define-derived-mode gitgit-grep-mode
  grep-mode
  gitgit-grep-mode-name
  "Major mode for gitgit grep"
  (define-key gitgit-grep-mode-map "!" 'gitgit-switch-to-texe)
  (define-key gitgit-grep-mode-map "\C-c\C-v"
              'gitgit-switch-to-status)
  (define-key compilation-mode-map "g" nil)
  (define-key compilation-minor-mode-map "g"
              nil)
  (define-key gitgit-grep-mode-map "g" 'texe-rerun)
  (define-key gitgit-grep-mode-map "\C-c\C-c"
              'texe-rerun)
  (gitgit-update-buffer-header-line (gitgit-get-current-branch)))

(provide 'gitgit-grep)
