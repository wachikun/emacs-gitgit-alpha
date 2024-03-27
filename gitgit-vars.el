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








# special について
#
# special と呼ばれる eval される文字列でコマンド実行時の挙動を指定可能。
# special は #@ からはじまるコメント行に記述する場合と、 " texe-special-comment-special-regexp
  "
# などのように専用の書式で記述する場合がある。
#
# 下記関数を指定できる。
# (同様の効果を持つものを複数指定した場合、どちらが優先されるかなどは規定していない)
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
# #@gitgit- ではじまるコメントは gitgit が内部的に使用する
#
# 下記の一覧は必須。
#     - " gitgit-texe-special-comment-status-initialize-script
                                                  "
#     - " gitgit-texe-special-comment-end-of-git-status
                                                  "
#     - " gitgit-texe-special-comment-end-of-files
                                                  "
#     - " gitgit-texe-special-comment-setup-modified-files-elisp
                                                  "
#
# 下記の一覧はオプション。
#     - " gitgit-texe-special-comment-modified-files
                                                  "
#     - " gitgit-texe-special-comment-recent-files
                                                  "




# special 未指定時の挙動を eval される文字列と regexp で指定
# (一括して設定するため、他とは異なる少し特殊な指定方法となっていることに注意)
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








# status 時に実行される script
#
# 実行された script の出力が status に反映される。
# --short --branch option が必須。
" gitgit-texe-special-comment-status-initialize-script
                                                  "-begin (texe-special-ignore-default)
git status --short --branch 2> /dev/null


# untracked files を無視する場合
# git status --short --branch --untracked-files=no 2> /dev/null
echo '" gitgit-texe-special-comment-end-of-git-status
                                                  "'




# recent files が追加される
#
# 不要ならばコメントにしても良い。
echo
echo '" gitgit-texe-special-comment-recent-files
                                                  "'




# (gitgit-setup-modified-files-update-files) 使用時、この位置に保存したファイル名が追加される
#
# (gitgit-setup-modified-files-update-status) 使用時はコメントにしても良い。
# echo
# echo '" gitgit-texe-special-comment-modified-files
                                                  "'




# git status では表示されないファイル一覧
#
# git ls-files でも ls でも、よく使うファイルを echo FILENAME しても、何でも可。
# 不要ならば完全にコメントにしても良い。


# ls-files を更新順ソート後に最新 16 個を表示
# 大きなプロジェクトで頻繁に触れるファイルが表示されるので便利。
# 但し、 Perl を使用する上 1 秒でタイムアウトすることに注意。
echo
echo '# git ls-files newest 16 files'
perl -e '$SIG{ALRM} = sub {exit 142}; alarm(1); my $c = 0; foreach (sort({-M $a <=> -M $b} split(/[\\r\\n]+/, `git ls-files`))) { print \"$_\\n\"; last if ++$c >= 16; }'


# ls-files をフィルタリングせずそのまま出力
#
# 小さなプロジェクトではおすすめ。
# echo '# git ls-files'
# git ls-files


# ls-files を拡張子でフィルタリング後 head -16 して出力
# echo \"# git ls-files | extension_filter | head -16\"
# git ls-files | egrep '\\.(txt|rb|py|pl|el|c|h|cxx|hpp|cpp|rs)$' | head -16


# find でも可
# find . -type d -name target -prune -o -type f -name '*.rs' -print


# echo でも可
# echo memo.org




# ここまでをファイル一覧としてパースする
#
# 必須なのでコメントにしてはならないことに注意。
echo
echo '" gitgit-texe-special-comment-end-of-files
                                                  "'




# status を更新した時刻
#
# 必須ではないので不要ならばコメントにしても良い。
echo
echo '# date'
date




# 通常の git status
#
# 状況を把握するためもので必須ではない。重い working directory ではコメントにしても可。
echo
echo '# git status --long'
git status --long
" gitgit-texe-special-comment-status-initialize-script
                                                  "-end








# modified files 時に実行される elisp の設定
" gitgit-texe-special-comment-setup-modified-files-elisp
                                                  "-begin
;; " gitgit-texe-special-comment-modified-files
                                                  " を更新するよう設定する
;;
;;     ファイル更新で git status を実行せず modified files を追加するだけなので高速だが、
;;     git status を反映させるには別途操作が必要。
;; (gitgit-setup-modified-files-update-files)

;; gitgit status を更新するよう設定する
;;
;;     ファイル更新と同時に git status を実行するので status は即時反映されるが、
;;     重い working directory ではおすすめできない。
(gitgit-setup-modified-files-update-status)
" gitgit-texe-special-comment-setup-modified-files-elisp
                                                  "-end




# gitgit status から実行される command の動作を指定
#
# gitgit-status-[command] のように記述することで指定可能。

#@gitgit-status-blame (texe-special-set-major-mode 'gitgit-blame-mode) (texe-special-set-point-min) (texe-special-ignore-default)
git blame '--date=format:%Y-%m-%d %H:%m' 2> /dev/null
# 日時は gitgit で参照していないので形式は任意
# ここでは横幅を取らない形で指定している。


#@gitgit-status-log (texe-special-set-major-mode 'gitgit-log-mode) (texe-special-set-point-min) (texe-special-ignore-default) (unless tmp-special-local-reload-p (texe-special-set-point-min))
git log -n 32 --stat-width=800 --graph --decorate=full --stat
# git log -n 32 --stat-width=800 --graph --decorate=full --patch-with-stat
# 大きなプロジェクトでは --graph などを外してしまうと高速
# git log -n 32 --stat-width=800


#@gitgit-status-diff (texe-special-set-major-mode 'gitgit-diff-mode) (texe-special-keep-select-texe-buffer) (texe-special-ignore-default) (unless tmp-special-local-reload-p (texe-special-set-point-min))
git diff


#@gitgit-status-commit (texe-special-keep-select-texe-buffer) (texe-special-ignore-default)
git commit
"))

(defconst gitgit-mode-default-texe (concat gitgit-mode-default-texe-user gitgit-mode-default-texe-system))

(defvar gitgit-temporary-file-directory-for-visited-file-name
  nil "set-visited-file-name で特定の directory を必要とするため、
temporary-file-directory とは別に userid ごとに用意しておく。
(user ごとに分けておかないと permission 関連の問題が発生することがある)")

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

(defcustom gitgit-texe-alist nil "directory ごとに texe を
\='((directory-a . texe-a) (directory-b . texe-b)) な alist で指定する。
directory は / で終端することに注意。
gitgit-texe-alist に存在しなければ gitgit-default-texe-directory 内に
buffer name から生成された texe が使用される。" :type 'string
:group 'gitgit-variables)

(defcustom gitgit-default-texe-directory "~/.emacs.d/.gitgit.texe/"
  "gitgit-default-texe-directory" :type 'string
  :group 'gitgit-variables)

(defcustom gitgit-internal-git-command "git"
  "gitgit-internal-git-command
内部用 git コマンドを設定する。" :type 'string
  :group 'gitgit-variables)

(defcustom gitgit-recent-files-limit 10 "gitgit-recent-files-limit
recent files の上限" :type 'integer
:group 'gitgit-variables)

(provide 'gitgit-vars)
