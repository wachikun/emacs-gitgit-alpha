# gitgit とは

*ALPHA version*

一旦 GitHub に上げないと、いつまでたっても完成しないので、とりあえず一旦上げているバージョンです。

「基本操作メニュー」 + 「CLI」という構成の Git インターフェースです。

下記のような特徴があります。

- `M-x gitgit` から呼ばれる gitgit status と呼ばれるメニューから、 Git コマンドを起動するまでもない基本操作をサポート
- Git CLI では面倒な、任意の複数ファイル操作は gitgit status のメニューで対応
- texe と呼ばれる単純なテキストから任意の CLI を直接起動
- 前作の mvc.el は Git 以外の VCS に対応するため複雑になり過ぎた反省から、 Git 専用に再設計




# Install

```console
$ cp -av texe.el gitgit-*.el /YOUR-ELISP-PATH/
```

```elisp
(require 'gitgit)
```




# How to use

* `M-x gitgit`

`.git` のあるディレクトリで `M-x gitgit` を実行すると、 gitgit status バッファが起動します。

gitgit status では `m` キーでマークしたファイルに対して、 Git コマンドを実行します。

複雑な操作は `!` キーで起動する texe と呼ばれるバッファで Git コマンドを直接実行します(後述)。


## gitgit status shortcuts

* `a add`

* `b blame`

* `c commit`

* `g reload status`

* `l log`

* `m mark`

* `r (`git restore` or `git rm --cached`)

* `u unmark`

* `D remove`

* `Q quit`

* `R rename`

* `= diff`

* `! call texe`




## Texe

コマンドを実行するための環境です。

Texe のファイル実体は `~/.emacs.d/.gitgit.texe/` に置かれ、`texe-mode` と呼ばれるモードが設定されている、ただのテキストファイルです。

通常の Shell で実行しないため、 History を汚染をしませんし、 History から消えてしまう心配もありません。

コマンドをお好みで並べておくことも可能ですので、定型処理にも強いという利点もあります。

`texe-mode` では `\C-c\C-c` でカーソル行のコマンドを実行できます。但し、対応しているのはコマンドからの出力のみなので、対話的なコマンドは実行できません。

コマンドは基本的には 1 行で記述しますが、下記のように `#@script-begin` と `#@script-end` で囲むことで複数行のコマンドを実行することも可能です。

```perl
#@script-begin
#!/usr/bin/perl -w

use strict;
use warnings;
use 5.010;

print "$0\n";
#@script-end
```

また、下記のように実行している Emacs 上で EmacsLisp を実行することもできます。

```elisp
#@elisp-begin
(message "hello!")
#@elisp-end
```

`texe-mode` のコメントは「#」または「;」です。




## 設定

gitgit status 自体も texe で設定します。

少し癖はありますが、 texe バッファの下の方、「system scripts」で検索すると、簡単に理解できると思います。




### 重いリポジトリ

Chromium や Firefox のような重量級のリポジトリでは、デフォルトの `git --no-pager log -n 32 --stat-width=800 --graph --decorate=full --patch-with-stat` では log 表示が遅くなるため、 texe バッファで下記のような設定をおすすめします。

```elisp
#@gitgit-git-log (progn (texe-special-set-major-mode 'gitgit-log-mode) (texe-special-set-point-min))
git --no-pager log -n 32 --stat-width=800
```




## special

special と呼ばれる特殊な行で、コマンド実行のふるまいを設定することができます。

*仕様変更予定。*

|関数名|説明|
|--|--|
|`(texe-special-buffer-name-suffix "suffix")`|出力バッファの suffix を指定|
|`(texe-special-buffer-name-suffix-time "suffix")`|出力バッファの suffix と時刻を指定|
|`(texe-special-set-major-mode 'major-mode-name)`|出力バッファの major-mode を指定|
|`(texe-special-keep-select-texe-buffer)`|出力バッファにフォーカスを移動しない|
|`(texe-special-no-display-process-buffer)`|出力バッファを表示しない|
|`(texe-special-set-point-min)`|出力バッファの point を `(point-min)` に設定する|
|`(texe-special-set-point-max)`|出力バッファの point を `(point-max)` に設定する|

*下記に変更予定。*

|関数名|説明|
|--|--|
|`suffix`: suffix|出力バッファの suffix を指定|
|`suffix-time`: suffix|出力バッファの suffix と時刻を指定|
|`major-mode`: mode|出力バッファの major-mode を指定|
|`retain-window-focus`|出力バッファにフォーカスを移動しない|
|`no-display-process-buffer`|出力バッファを表示しない|
|`set-point: min`|出力バッファの point を `(point-min)` に設定する|
|`set-point: max`|出力バッファの point を `(point-max)` に設定する|

上記の他に、特殊なコマンドとして `@FORCE-YES` というものがあります。これを先頭に付加することでコマンド実行時の `yes-or-no-p` を省略します。


### 出力バッファの suffix を "-DIFF" にして major-mode を gitgit-diff-mode にする

*仕様変更予定。*

```elisp
#@(progn (texe-special-set-major-mode 'gitgit-diff-mode) (texe-special-buffer-name-suffix "-DIFF"))
git diff
```
