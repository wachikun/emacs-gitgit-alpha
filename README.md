# gitgit とは

*ALPHA version*

一旦 GitHub に上げないと、いつまでたっても完成しないので、とりあえず一旦上げているバージョンです。

「基本操作メニュー」 + 「CLI」という構成の Git インターフェースです。

下記のような特徴があります。

- `M-x gitgit` から呼ばれる gitgit status と呼ばれるメニューから、 Git コマンドを起動するまでもない基本操作をサポート
- 標準の Git コマンドでは面倒な、任意の複数ファイル操作は gitgit status のメニューで対応
- texe と呼ばれる単純なテキストから任意の CLI を直接起動可能
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

ここで操作できるのは、よく使うコマンドのみで、 push/pull や、その他複雑な操作は `!` キーで起動する texe と呼ばれるバッファで Git コマンドを直接実行します(後述)。


## gitgit status shortcuts

* `a add`

* `b blame`

* `c commit`

* `g rerun status`

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

Texe のファイル実体は `~/.emacs.d/.gitgit.texe/` に置かれ、`texe-mode` と呼ばれるモードが設定されている、テキストファイルです。

コマンドを通常の Shell 環境から実行しないため、 History を汚染をしませんし、大切なコマンドが History から消えてしまう心配もありません。

コマンドをお好みで並べておくことも可能ですので、定型処理にも強いという利点もあります。

`texe-mode` では `\C-c\C-c` でカーソル行のコマンドを実行できます。但し、対応しているのはコマンドからの出力のみなので、対話的なコマンドは実行できません。

コマンドは基本的には 1 行で記述しますが、下記のように `#@script-begin` と `#@script-end` で囲むことで複数行のコマンドを実行することも可能です。

```rust
#@script-begin
#!/usr/bin/env rust-script
println!("{:?}", std::env::args());
#@script-end
```

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

Chromium や Firefox のような重量級のリポジトリでは、 default の `git --no-pager log -n 32 --stat-width=800 --graph --decorate=full --patch-with-stat` では log 表示が遅くなるため、 texe バッファで下記のような設定をおすすめします。

```elisp
#@gitgit-status-log (progn (texe-special-set-major-mode 'gitgit-log-mode) (texe-special-set-point-min))
git --no-pager log -n 32 --stat-width=800
```




## special

special と呼ばれる特殊な行で、コマンド実行のふるまいを設定することができます。

|関数名|説明|
|--|--|
|`(texe-special-set-use-default-p enable-p)`|enable-p が t ならば正規表現で指定される文字列を含むコマンドに対する default 設定を適用する (dafault: t)|
|`(texe-special-set-call-texe-callback-p enable-p)`|enable-p が t ならば texe 実行後の callback を呼び出す (dafault: t)|
|`(texe-special-set-user-callback callback)`|texe 実行後の callback を設定 (dafault: nil)|
|`(texe-special-set-buffer-name-suffix "suffix")`|出力バッファの suffix を指定 (dafault: nil)|
|`(texe-special-set-buffer-name-suffix-time "suffix")`|出力バッファの suffix と時刻を指定| (dafault: nil)
|`(texe-special-set-major-mode 'major-mode-name)`|出力バッファの major-mode を指定 (dafault: nil)|
|`(texe-special-set-keep-select-texe-buffer-p enable-p)`|enable-p が t ならばフォーカスを texe window のままにする (dafault: nil)|
|`(texe-special-set-display-process-buffer-p enable-p)`|enable-p が t ならば出力バッファを表示する (dafault: t)|
|`(texe-special-set-goto-point-min-p enable-p)`|enable-p が t ならば出力バッファの point を `(point-min)` に設定する (dafault: nil)|
|`(texe-special-set-goto-point-max-p enable-p)`|enable-p が t ならば出力バッファの point を `(point-max)` に設定する (dafault: nil)|
|`(texe-special-set-append-shell-command COMMAND)`|シェルコマンドの最後に COMMAND を追加する (dafault: nil)|
|`(texe-special-rerun-p)`|rerun ならば t を返す|

上記の他に、特殊なコマンドとして `@FORCE-YES` というものがあります。これを先頭に付加することでコマンド実行時の `yes-or-no-p` を省略します。

(texe-special-ignore-default) が指定されない限り、正規表現で指定される文字列を含むコマンドに対して default の special を適用しようとします。ここで指定される正規表現はあえて緩くしてあるため、意図しない default 設定が適用されることがあります。


### 出力バッファの suffix を "-DIFF" にして major-mode を gitgit-diff-mode にする

```elisp
#@(texe-special-set-major-mode 'gitgit-diff-mode) (texe-special-set-buffer-name-suffix "-DIFF")
git diff
```
