;;; texe.el -- Command Cheat Sheet -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

;; args-alist
;; 
;; - file-list
;;     command $B9=C[A0$N(B file list $B!#(B
;; - script-tmpfile
;;     script $B<B9TMQ$N(B tmpfile $B!#(B
;; - i-texe-buffer-name
;;     texe-run-start-process $B$G@_Dj$5$l$k!#(B
;; - i-from-texe
;;     texe $B$+$i5/F0$7$?>l9g$K@_Dj$5$l$k!#(B

;; https://github.com/wachikun/emacs-gitgit-alpha

;;; Code:

(require 'texe-interactive)
(require 'texe-run-start-process)
(require 'texe-point)
(require 'texe-process)
(require 'texe-specials)

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


;; minor-mode $B$J$N$G@hF,$N(B " " $B$OI,?\$G$"$k$3$H$KCm0U(B
(defconst texe--mode-name " Texe")

(defconst texe-special-comment-special-regexp
  "#@texe-default-special-regexp" "")

(defconst texe--special-comment-regexp-special-regexp (concat "^" texe-special-comment-special-regexp
                                                              "-")
  "")

(defvar texe-process-running-p-hash (make-hash-table :test 'equal))

(defvar texe-buffer-not-found-supplementary-message
  "" "texe buffer $B$,8+IU$+$i$J$$>l9g$NJdB-%a%C%;!<%8(B")

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
            'texe-process-mode-texe)
(define-key texe-mode-process-mode-map "g"
            'texe-rerun)

(define-minor-mode texe-mode
  "Toggle Texe mode in the current buffer."
  :lighter texe--mode-name
  (set (make-local-variable 'texe-mode-local-run-core-special-alist)
       nil)
  (set (make-local-variable 'texe-mode-local-default-special-regexp-list)
       nil)
  (font-lock-add-keywords nil
                          '(("^[ \t]*\\(#@.*\\)" 1 'texe--face-special-comment)
                            ("^[ \t]*\\(#[^@].*\\)" 1 'texe--face-comment)
                            ("^[ \t]*\\(;.*\\)" 1 'font-lock-comment-face)))
  (if texe-mode
      (use-local-map texe-mode-map)
    (use-local-map nil)))

(defun texe-get-line ()
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(defun texe-set-line-face-if-need ()
  (let* ((line (texe-get-line))
         (face (cond
                ((string-match "^#@" line) 'texe--face-special-comment)
                ((string-match "^#" line) 'texe--face-comment)
                (t nil))))
    (when face
      (let ((backup-buffer-read-only buffer-read-only))
        (setq buffer-read-only nil)
        (add-text-properties (line-beginning-position)
                             (line-end-position)
                             (list 'face face))
        (setq buffer-read-only backup-buffer-read-only)))))

(defun texe-get-next-buffer-name (buffer-name-list buffer-name)
  (let ((result (cdr (member buffer-name buffer-name-list))))
    (if result
        (car result)
      (nth 0 buffer-name-list))))

(defun texe-get-previous-buffer-name (buffer-name-list buffer-name)
  (let* ((list-length (length buffer-name-list))
         (membered-list (member buffer-name buffer-name-list))
         (membered-length (length membered-list))
         (previous-index (1- (- list-length membered-length))))
    (if (< previous-index 0)
        (nth (1- list-length)
             buffer-name-list)
      (nth previous-index buffer-name-list))))

(defun texe-setup-default-special-regexp ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat texe--special-comment-regexp-special-regexp
                                     "begin")
                             nil
                             t)
      (beginning-of-line)
      (let ((special-command-list (texe--get-region-special-begin-and-command)))
        (if (and special-command-list
                 (listp special-command-list))
            (let* ((command (replace-regexp-in-string "\n$"
                                                      ""
                                                      (nth 1 special-command-list)))
                   (default-special-regexp-list (split-string command "\n")))
              (if (= (% (length default-special-regexp-list)
                        2) 0)
                  (setq texe-mode-local-default-special-regexp-list
                        default-special-regexp-list)
                (message "illegal pair")
                (throw 'error nil)))
          (message "illegal region")
          (throw 'error nil))))))

(defun texe-get-current-line-command ()
  (catch 'error
    (let (command special-command-list)
      (setq special-command-list (texe--get-region-special-begin-and-command))
      (if special-command-list
          (if (listp special-command-list)
              (setq command (nth 1 special-command-list))
            (message "illegal region")
            (throw 'error nil))
        (setq special-command-list (texe--get-special-and-command))
        (if special-command-list
            (setq command (nth 1 special-command-list))
          (setq command (texe-get-line))))
      command)))

(defun texe--get-texe-buffer-list ()
  (let (texe-buffer-list)
    (mapc #'(lambda (buffer)
              (with-current-buffer buffer
                (when (and (boundp 'texe-mode)
                           (symbol-value 'texe-mode))
                  (setq texe-buffer-list (append texe-buffer-list
                                                 (list (buffer-name)))))))
          (buffer-list))
    (sort texe-buffer-list 'string<)))

(defun texe--get-window-information-list (buffer)
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

(defun texe--use-replace-buffer-contents-p (source-max)
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

(defun texe--get-region-special-begin-and-command ()
  "#@*-begin
#@*-end
$B$G0O$^$l$F$$$l$P(B (\"#@*-begin\" \"command\") $B$N%j%9%H$r!"0O$^$l$F$$$J$1$l$P(B nil $B$r!"(B #@* $B$,(B
$B0lCW$7$F$$$J$1$l$P(B t $BJV$9!#(B"
  (catch 'return
    (let (special-begin special-end command-start command)
      (save-excursion
        (when (string-match "^#@.+-end$" (texe-get-line))
          (forward-line -1)
          (when (string-match "^#@.+-end$" (texe-get-line))
            (throw 'return nil)))
        (when (string-match "^#@.+-begin" (texe-get-line))
          (end-of-line))
        (when (re-search-backward "^#@" nil t)
          (setq special-begin (texe-get-line))
          (when (string-match ".-begin" special-begin)
            (forward-line 1)
            (setq command-start (point))
            (when (re-search-forward "^#@" nil t)
              (setq special-end (texe-get-line))
              (when (string-match ".-end$" special-end)
                (let (special-begin-special special-end-special)
                  (string-match "^([^-]+)-" special-begin)
                  (setq special-begin-special (match-string 1 special-begin))
                  (string-match "^([^-]+)-" special-end)
                  (setq special-end-special (match-string 1 special-end))
                  (if (string= special-begin-special special-end-special)
                      (progn
                        (setq command (buffer-substring command-start
                                                        (line-beginning-position)))
                        (list special-begin command))
                    t))))))))))

(defun texe--get-special-and-command ()
  "$B8=:_$N9T$,(B #@* $B$N<!$+(B #@* $B$HF1$8$J$i$P(B (\"#@*\" \"command\") $B$N%j%9%H$r!"$=$&$G$J$1$l$P(B
nil $B$rJV$9!#C"$7!"(B #@* $B$,(B -begin $B$^$?$O(B -end $B$G=*$($F$$$?>l9g$b(B nil $B$rJV$9(B"
  (save-excursion
    (let (tmp special command)
      (setq tmp (texe-get-line))
      (if (and (string-match "^#@" tmp)
               (not (string-match ".-begin$" tmp))
               (not (string-match ".-end$" tmp)))
          (progn
            (setq special tmp)
            (forward-line 1)
            (setq command (texe-get-line))
            (list special command))
        (forward-line -1)
        (setq special (texe-get-line))
        (when (and (string-match "^#@" special)
                   (not (string-match ".-begin$" special))
                   (not (string-match ".-end$" special)))
          (setq command tmp)
          (list special command))))))

(provide 'texe)
