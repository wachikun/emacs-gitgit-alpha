;;; gitgit-vars.el -- Git User Interface -*- lexical-binding: t; coding: iso-2022-jp; -*-

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

(defvar gitgit-version-string "gitgit version 0.1")

(defconst gitgit-texe-diff-hash "d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f")

(defconst gitgit-texe-file-name-prefix "*gitgit*")
(defconst gitgit-texe-status-buffer-suffix
  "status")
(defconst gitgit-texe-status-buffer-regexp (concat "^"
                                                   (regexp-quote gitgit-texe-file-name-prefix)
                                                   ".+? "
                                                   gitgit-texe-status-buffer-suffix
                                                   "$"))

(defconst gitgit-texe-special-comment-status-initialize-script
  "#@gitgit-status-initialize-script")
(defconst gitgit-texe-special-comment-setup-modified-files-elisp
  "#@gitgit-setup-modified-files-elisp")
(defconst gitgit-texe-special-comment-end-of-git-status
  "#@gitgit-end-of-git-status")
(defconst gitgit-texe-special-comment-recent-files
  "#@gitgit-recent-files")
(defconst gitgit-texe-special-comment-modified-files
  "#@gitgit-modified-files")
(defconst gitgit-texe-special-comment-end-of-files
  "#@gitgit-end-of-files")
(defconst gitgit-texe-special-system-scripts
  "#@ ================ system scripts ================")

(defconst gitgit-status-process-running-message-delay-second
  0.3)

(defconst gitgit-mode-default-texe-user (concat "# This is gitgit-mode-default-texe.
#
# C-cC-c to run current line command
#

git remote show origin

git log -n 8 --graph --decorate=full --stat
git log -n 8 --graph --decorate=full --patch-with-stat




# BRANCH template
git merge --no-ff BRANCH
git checkout BASE
git rebase --interactive BASE
git diff master BRANCH
git checkout -b BRANCH




# git archive
#@script-begin
NAME=$(basename $(pwd)).$(date '+%Y%m%d%H%M%S')
git archive --prefix=$NAME/ --output=/tmp/$NAME.tar HEAD
gzip -9 /tmp/$NAME.tar
ls -l /tmp/$NAME.tar.gz
#@script-end




#@elisp-begin
(message \"hello! texe-buffer-name=%s\" texe-buffer-name)
(with-current-buffer texe-buffer-name
  (gitgit-status--view-status-from-texe-buffer nil))
#@elisp-end








# special $B$K$D$$$F(B
#
# special $B$H8F$P$l$k(B eval $B$5$l$kJ8;zNs$G%3%^%s%I<B9T;~$N5sF0$r;XDj2DG=!#(B
# special $B$O(B #@ $B$+$i$O$8$^$k%3%a%s%H9T$K5-=R$9$k>l9g$H!"(B " texe-special-comment-special-regexp
  "
# $B$J$I$N$h$&$K@lMQ$N=q<0$G5-=R$9$k>l9g$,$"$k!#(B
#
# $B2<5-4X?t$r;XDj$G$-$k!#(B
# ($BF1MM$N8z2L$r;}$D$b$N$rJ#?t;XDj$7$?>l9g!"$I$A$i$,M%@h$5$l$k$+$J$I$O5,Dj$7$F$$$J$$(B)
#     - (texe-special-buffer-name-suffix \"suffix\")
#     - (texe-special-buffer-name-suffix-time \"suffix\")
#     - (texe-special-set-major-mode 'major-mode-name)
#     - (texe-special-keep-select-texe-buffer)
#     - (texe-special-no-display-process-buffer)
#     - (texe-special-set-point-min)
#     - (texe-special-set-point-max)
# (ex.
# #@(texe-special-set-major-mode 'gitgit-diff-mode) (texe-special-buffer-name-suffix \"-diff\")




"))

(defconst gitgit-mode-default-texe-system (concat gitgit-texe-special-system-scripts
                                                  "
#
# #@gitgit- $B$G$O$8$^$k%3%a%s%H$O(B gitgit $B$,FbItE*$K;HMQ$9$k(B
#
# $B2<5-$N0lMw$OI,?\!#(B
#     - " gitgit-texe-special-comment-status-initialize-script
                                                  "
#     - " gitgit-texe-special-comment-end-of-git-status
                                                  "
#     - " gitgit-texe-special-comment-end-of-files
                                                  "
#     - " gitgit-texe-special-comment-setup-modified-files-elisp
                                                  "
#
# $B2<5-$N0lMw$O%*%W%7%g%s!#(B
#     - " gitgit-texe-special-comment-modified-files
                                                  "
#     - " gitgit-texe-special-comment-recent-files
                                                  "




# special $BL$;XDj;~$N5sF0$r(B eval $B$5$l$kJ8;zNs$H(B regexp $B$G;XDj(B
# ($B0l3g$7$F@_Dj$9$k$?$a!"B>$H$O0[$J$k>/$7FC<l$J;XDjJ}K!$H$J$C$F$$$k$3$H$KCm0U(B)
" texe-special-comment-special-regexp "-begin
(texe-special-set-major-mode 'gitgit-diff-mode) (texe-special-buffer-name-suffix \"-diff\") (texe-special-keep-select-texe-buffer) (unless tmp-special-local-reload-p (texe-special-set-point-min))
 ?diff

(texe-special-set-major-mode 'gitgit-log-mode) (texe-special-buffer-name-suffix \"-log\") (unless tmp-special-local-reload-p (texe-special-set-point-min))
 log

(texe-special-set-major-mode 'gitgit-blame-mode) (texe-special-buffer-name-suffix \"-blame\") (texe-special-set-point-min)
 blame

(texe-special-keep-select-texe-buffer) (texe-special-buffer-name-suffix \"-branch\")
 branch

(texe-special-keep-select-texe-buffer) (texe-special-ignore-process-running) (texe-special-buffer-name-suffix \"-commit\")
 commit

(texe-special-keep-select-texe-buffer) (texe-special-buffer-name-suffix \"-checkout\")
 checkout

(texe-special-keep-select-texe-buffer) (texe-special-buffer-name-suffix \"-switch\")
 switch

(texe-special-keep-select-texe-buffer) (texe-special-ignore-process-running) (texe-special-buffer-name-suffix \"-merge\")
 merge

(texe-special-keep-select-texe-buffer) (texe-special-ignore-process-running) (texe-special-buffer-name-suffix \"-rebase\")
 rebase

(texe-special-keep-select-texe-buffer) (texe-special-buffer-name-suffix \"-push\")
 push

(texe-special-keep-select-texe-buffer) (texe-special-buffer-name-suffix \"-pull\")
 pull

(texe-special-set-major-mode 'gitgit-grep-mode) (texe-special-buffer-name-suffix \"-grep\") (unless tmp-special-local-reload-p (texe-special-set-point-min))
 ?\\(grep\\|rg\\|hw\\|ag\\) 
" texe-special-comment-special-regexp "-end








# status $B;~$K<B9T$5$l$k(B script
#
# $B<B9T$5$l$?(B script $B$N=PNO$,(B status $B$KH?1G$5$l$k!#(B
# --short --branch option $B$,I,?\!#(B
" gitgit-texe-special-comment-status-initialize-script
                                                  "-begin (texe-special-ignore-default)
git status --short --branch 2> /dev/null


# untracked files $B$rL5;k$9$k>l9g(B
# git status --short --branch --untracked-files=no 2> /dev/null
echo '" gitgit-texe-special-comment-end-of-git-status
                                                  "'




# recent files $B$,DI2C$5$l$k(B
#
# $BITMW$J$i$P%3%a%s%H$K$7$F$bNI$$!#(B
echo
echo '" gitgit-texe-special-comment-recent-files
                                                  "'




# (gitgit-setup-modified-files-update-files) $B;HMQ;~!"$3$N0LCV$KJ]B8$7$?%U%!%$%kL>$,DI2C$5$l$k(B
#
# (gitgit-setup-modified-files-update-status) $B;HMQ;~$O%3%a%s%H$K$7$F$bNI$$!#(B
# echo
# echo '" gitgit-texe-special-comment-modified-files
                                                  "'




# git status $B$G$OI=<($5$l$J$$%U%!%$%k0lMw(B
#
# git ls-files $B$G$b(B ls $B$G$b!"$h$/;H$&%U%!%$%k$r(B echo FILENAME $B$7$F$b!"2?$G$b2D!#(B
# $BITMW$J$i$P40A4$K%3%a%s%H$K$7$F$bNI$$!#(B


# ls-files $B$r99?7=g%=!<%H8e$K:G?7(B 16 $B8D$rI=<((B
# $BBg$-$J%W%m%8%'%/%H$GIQHK$K?($l$k%U%!%$%k$,I=<($5$l$k$N$GJXMx!#(B
# $BC"$7!"(B Perl $B$r;HMQ$9$k>e(B 1 $BIC$G%?%$%`%"%&%H$9$k$3$H$KCm0U!#(B
echo
echo '# git ls-files newest 16 files'
perl -e '$SIG{ALRM} = sub {exit 142}; alarm(1); my $c = 0; foreach (sort({-M $a <=> -M $b} split(/[\\r\\n]+/, `git ls-files`))) { print \"$_\\n\"; last if ++$c >= 16; }'


# ls-files $B$r%U%#%k%?%j%s%0$;$:$=$N$^$^=PNO(B
#
# $B>.$5$J%W%m%8%'%/%H$G$O$*$9$9$a!#(B
# echo '# git ls-files'
# git ls-files


# ls-files $B$r3HD%;R$G%U%#%k%?%j%s%08e(B head -16 $B$7$F=PNO(B
# echo \"# git ls-files | extension_filter | head -16\"
# git ls-files | egrep '\\.(txt|rb|py|pl|el|c|h|cxx|hpp|cpp|rs)$' | head -16


# find $B$G$b2D(B
# find . -type d -name target -prune -o -type f -name '*.rs' -print


# echo $B$G$b2D(B
# echo memo.org




# $B$3$3$^$G$r%U%!%$%k0lMw$H$7$F%Q!<%9$9$k(B
#
# $BI,?\$J$N$G%3%a%s%H$K$7$F$O$J$i$J$$$3$H$KCm0U!#(B
echo
echo '" gitgit-texe-special-comment-end-of-files
                                                  "'




# status $B$r99?7$7$?;~9o(B
#
# $BI,?\$G$O$J$$$N$GITMW$J$i$P%3%a%s%H$K$7$F$bNI$$!#(B
echo
echo '# date'
date




# $BDL>o$N(B git status
#
# $B>u67$rGD0.$9$k$?$a$b$N$GI,?\$G$O$J$$!#=E$$(B working directory $B$G$O%3%a%s%H$K$7$F$b2D!#(B
echo
echo '# git status --long'
git status --long
" gitgit-texe-special-comment-status-initialize-script
                                                  "-end








# modified files $B;~$K<B9T$5$l$k(B elisp $B$N@_Dj(B
" gitgit-texe-special-comment-setup-modified-files-elisp
                                                  "-begin
;; " gitgit-texe-special-comment-modified-files
                                                  " $B$r99?7$9$k$h$&@_Dj$9$k(B
;;
;;     $B%U%!%$%k99?7$G(B git status $B$r<B9T$;$:(B modified files $B$rDI2C$9$k$@$1$J$N$G9bB.$@$,!"(B
;;     git status $B$rH?1G$5$;$k$K$OJLESA`:n$,I,MW!#(B
;; (gitgit-setup-modified-files-update-files)

;; gitgit status $B$r99?7$9$k$h$&@_Dj$9$k(B
;;
;;     $B%U%!%$%k99?7$HF1;~$K(B git status $B$r<B9T$9$k$N$G(B status $B$OB(;~H?1G$5$l$k$,!"(B
;;     $B=E$$(B working directory $B$G$O$*$9$9$a$G$-$J$$!#(B
(gitgit-setup-modified-files-update-status)
" gitgit-texe-special-comment-setup-modified-files-elisp
                                                  "-end




# gitgit status $B$+$i<B9T$5$l$k(B command $B$NF0:n$r;XDj(B
#
# gitgit-status-[command] $B$N$h$&$K5-=R$9$k$3$H$G;XDj2DG=!#(B

#@gitgit-status-blame (texe-special-set-major-mode 'gitgit-blame-mode) (texe-special-set-point-min) (texe-special-ignore-default)
git blame '--date=format:%Y-%m-%d %H:%m' 2> /dev/null
# $BF|;~$O(B gitgit $B$G;2>H$7$F$$$J$$$N$G7A<0$OG$0U(B
# $B$3$3$G$O2#I}$r<h$i$J$$7A$G;XDj$7$F$$$k!#(B


#@gitgit-status-log (texe-special-set-major-mode 'gitgit-log-mode) (texe-special-set-point-min) (texe-special-ignore-default) (unless tmp-special-local-reload-p (texe-special-set-point-min))
git log -n 32 --stat-width=800 --graph --decorate=full --stat
# git log -n 32 --stat-width=800 --graph --decorate=full --patch-with-stat
# $BBg$-$J%W%m%8%'%/%H$G$O(B --graph $B$J$I$r30$7$F$7$^$&$H9bB.(B
# git log -n 32 --stat-width=800


#@gitgit-status-diff (texe-special-set-major-mode 'gitgit-diff-mode) (texe-special-keep-select-texe-buffer) (texe-special-ignore-default) (unless tmp-special-local-reload-p (texe-special-set-point-min))
git diff


#@gitgit-status-commit (texe-special-keep-select-texe-buffer) (texe-special-ignore-default)
git commit
"))

(defconst gitgit-mode-default-texe (concat gitgit-mode-default-texe-user gitgit-mode-default-texe-system))

(defvar gitgit-temporary-file-directory-for-visited-file-name
  nil "set-visited-file-name $B$GFCDj$N(B directory $B$rI,MW$H$9$k$?$a!"(B
temporary-file-directory $B$H$OJL$K(B userid $B$4$H$KMQ0U$7$F$*$/!#(B
(user $B$4$H$KJ,$1$F$*$+$J$$$H(B permission $B4XO"$NLdBj$,H/@8$9$k$3$H$,$"$k(B)")

(defface gitgit--face-git-default-directory '((((type x w32 mac ns)
                                                (class color)
                                                (background light))
                                               (:foreground "gray20" :bold t))
                                              (((type x w32 mac ns)
                                                (class color)
                                                (background dark))
                                               (:foreground "gray30" :bold t)))
  "branch"
  :group 'gitgit-faces)

(defface gitgit--face-git-branch '((((type x w32 mac ns)
                                     (class color)
                                     (background light))
                                    (:foreground "gray20" :bold t))
                                   (((type x w32 mac ns)
                                     (class color)
                                     (background dark))
                                    (:foreground "gray30" :bold t)))
  "branch"
  :group 'gitgit-faces)

(defface gitgit--face-git-branch-master '((((type x w32 mac ns)
                                            (class color)
                                            (background light))
                                           (:foreground "red4" :bold t))
                                          (((type x w32 mac ns)
                                            (class color)
                                            (background dark))
                                           (:foreground "red2" :bold t)))
  "branch master"
  :group 'gitgit-faces)

(defvar gitgit--initial-directory-hash (make-hash-table :test 'equal))
(defvar gitgit--buffer-name-count-hash (make-hash-table :test 'equal))

(defcustom gitgit-texe-alist nil "directory $B$4$H$K(B texe $B$r(B
\='((directory-a . texe-a) (directory-b . texe-b)) $B$J(B alist $B$G;XDj$9$k!#(B
directory $B$O(B / $B$G=*C<$9$k$3$H$KCm0U!#(B
gitgit-texe-alist $B$KB8:_$7$J$1$l$P(B gitgit-default-texe-directory $BFb$K(B
buffer name $B$+$i@8@.$5$l$?(B texe $B$,;HMQ$5$l$k!#(B" :type 'string
:group 'gitgit-variables)

(defcustom gitgit-default-texe-directory "~/.emacs.d/.gitgit.texe/"
  "gitgit-default-texe-directory" :type 'string
  :group 'gitgit-variables)

(defcustom gitgit-internal-git-command "git"
  "gitgit-internal-git-command
$BFbItMQ(B git $B%3%^%s%I$r@_Dj$9$k!#(B" :type 'string
  :group 'gitgit-variables)

(defcustom gitgit-recent-files-limit 10 "gitgit-recent-files-limit
recent files $B$N>e8B(B" :type 'integer
:group 'gitgit-variables)

(provide 'gitgit-vars)
