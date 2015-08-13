---
title: Hakyll, stack, Travis CI, Github でブログを管理する
description: 初期設定
tags: Haskell, Hakyll, stack, Travis
---

やった事をまとめると、以下の通り。

- StackでHaskellのパッケージ(Hakyll)を管理する。
- markdownでブログを書き、Hakyllでhtmlファイルに変換する。
- 実際には、TravisがHakyllを使ってコンパイルしhtmlファイルに変換する。
- ソースファイルはGithubの公開リポジトリで管理し、Github Pagesで公開する。

できたのが[本ブログ](https://github.com/335g/335g.github.io)。

## Stack

[Stack](https://github.com/commercialhaskell/stack)は、
Haskellのパッケージをビルドしたりインストールしたりするツールです。
[tanakh](https://twitter.com/tanakh)さんによる
[紹介記事](http://qiita.com/tanakh/items/6866d0f570d0547df026)をきっかけに知りました。
Haskell力のない自分にはcabal hellを乗り越えられる自信が無かったという理由と、
タイムリーなので使ってみたいという理由から選択してみました。

## Hakyll

[Hakyll](http://jaspervdj.be/hakyll/)は、
Haskellによる静的htmlページ生成ツールです。
RubyによるJekyllにインスパイアされた作られたとか。

```
hakyll-init "site"
```

とすることで`site`というディレクトリ内に必要なファイル一式が生成されます。
あとは、

```
ghc --make site.hs
./site build
```

と実行すれば`_site`ディレクトリ内にhtml及びcss等の必要ファイル一式が生成されます。
具体的には`site.hs`を見ながら[チュートリアル](http://jaspervdj.be/hakyll/tutorials.html)を読んでみてください。
シンプルな構成になっているのでなんとなくであればすぐに理解できると思います。(自分もまだ理解不足ですが)

## Travis

[Travis CI](https://travis-ci.org)はCIを行うためのサービスです。
[Stackを用いるための設定方法](https://github.com/commercialhaskell/stack/wiki/Travis)が公開されています。

## Github Pages

[Github Pages](https://pages.github.com)はユーザやリポジトリ毎に提供されるWebページ公開サービスです。
ソースはGithubのリポジトリで管理します。注意が必要なのは最上階層に`index.html`が必要ということでしょうか。
Hakyllでは`_site`にhtmlが生成されるからです。

## 記事作成 〜 公開 の流れ

Hakyllでは`_site`ディレクトリにコードを生成するが、最上階層に`index.html`が必要という問題を
git submoduleで吸収します。また、ついでにTravisにコード生成もやってもらいます。

- あらかじめ`_site`ディレクトリをsubmoduleに追加しておく
- `source`ブランチ: markdownで記事を書く
- `source`ブランチ: hakyllでコンパイルし正しく表示されるか確認する (省略可)
- `source`ブランチ: githubにpushする
- `master`ブランチ: Travisがコンパイルしhtmlファイルを生成する(`_site`ディレクトリに)
- `master`ブランチ: Travisがgithubにpushする
- Github Pagesに公開される

つまり、記事生成を`source`ブランチで行ってgithubにpushすれば、
`master`ブランチに必要なファイルが作成され記事が公開されるという流れです。

### stackをインストール

[releases](https://github.com/commercialhaskell/stack/releases)から最新版をダウンロード。
パスが通っているディレクトリに配置します。

### ビルドする

```
cabal init
```

まず`.cabal`ファイルを作っておきます。
注意するところとしては、ライセンスをきちんと設定しておかないと
(ディレクトリに`LILCENSE`が無いと？)後の`stack build`部でこけてしまいます。  

次に、生成された`.cabal`ファイルに`hakyll`依存を明記します。

```
build-depends: hakyll >=4.5
```

Stackageで管理されている安定verが4.6.9.0でしたので、
それが満たせるような表記になっていれば良さそうです。

```
stack init
```

これで`stack.yaml`が生成されます。  

まずは`stack.yaml`をいじらずに`stack build`をしてみます。すると現時点のバージョンでは、
`hfsevents: needed (>=0.1.3)`と怒られてしまいます。ログの下の方に

```
Recommended action: try adding the following to your extra-deps in .../stack.yaml
- hfsevents-0.1.5
```
とあるので、`stack.yaml`の`extra-deps`の項に追記します。

```
flags: {}
packages:
- '.'
extra-deps:
- hfsevents-0.1.5
resolver: lts-2.21
```

以上で

```
stack build
```

とすると必要なパッケージのインストールが始まります。(初回はちょっと長め)  

これまでsyntax highlightが必要な場合は`cabal install --reinstall -fhighlighting pandoc`とか
してたと思います。おそらく`stack.yaml`の`flags`を設定してやればうまくいくかと思うのですが、
私はそれに気づかずにインストールしてしまいました。今現在再インストールするような仕組みは無く、
何を消去すれば再インストールできるのかわからなかったため、
とりあえず[highlight.js](https://highlightjs.org)を使っています。  

インストール終了後に

```
stack exec hakyll-init weblog
```

とすることで、`weblog`フォルダに必要ファイル一式が生成されます。
ここまででビルドできるようになるのですが、まずはgit submoduleやTravisの設定を行ってしまいます。

### git submodule & Travisの設定をする

`weblog/_site`をサブモジュールに追加します。
そのためまずは、Githubのレポジトリ(`master`ブランチ)に空の状態でpushします。

```
git init
git commit --allow-empty -m 'first commit'
git remote add origin git@github.com:<account>/<account>.github.io.git
git push origin master

git checkout -b source
git submodule add git@github.com:<account>/<account>.github.io.git weblog/_site
```

作成された`.gitmodule`を`Travis`から読めるようにするため、
urlを修正します。

```
https://github.com/<account name>/<account name>.github.io.git
```

必要無いファイルは管理しないように`.gitignore`を作成します。

```
weblog/_site
weblog/_cache
weblog/site*
!weblog/site.hs
.stack-work
```

`Travis`の管理画面で、
`<account>/<account>.github.io`のスイッチをonにすることでレポジトリをアクティブにします。  

また、歯車アイコンから設定画面へ行き、
`Build only if .travis.yml is present`のスイッチをonにしておきます。  

次に`.travis.yml`を作ります。
環境変数に`GH_TOKEN`と`GH_EMAIL`をセキュアな状態で入力する必要があるため、
`travis`コマンドを使います。

```
travis encrypt -r <account>/<account>.github.io.git GH_EMAIL=<email address>
travis encrypt -r <account>/<account>.github.io.git GH_TOKEN=<github token>
```

ここで使う`<github token>`はGithubで生成したものを使います。
また、`travis`コマンドは`gem`を使ってあらかじめインストールしておきます。

```
sudo gem install travis
```

`.travis.yml`はこんな感じにしました。

```

language: haskell

sudo: false
cache:
  directories:
    - $HOME/.stack/

branches:
  only:
    - source

env:
  global:
    - GH_NAME="335g.travis"
    - secure: "*******" # token
    - secure: "*******" # email

before_install:
  - mkdir -p ~/.local/bin
  - export PATH=~/.local/bin:$PATH
  - travis_retry curl -L https://github.com/commercialhaskell/stack/releases/download/v0.1.2.0/stack-0.1.2.0-x86_64-linux.gz | gunzip > ~/.local/bin/stack
  - chmod a+x ~/.local/bin/stack
  - export PATH=~/opt/ghc/7.8.4/bin:$PATH
  - git submodule foreach --recursive 'git checkout master; git ls-files | grep -v README | grep -v CNAME | xargs -r git rm'

addons:
  apt:
    sources:
      - hvr-ghc
    packages:
      - ghc-7.8.4

install:
  - stack setup --no-terminal
  - stack build --only-snapshot --no-terminal

before_script:
  - git config --global user.name "$GH_NAME"
  - git config --global user.email "$GH_EMAIL"

script:
  - stack build --no-terminal
  - cd weblog
  - stack ghc site.hs
  - ./site build

after_success:
  - cd _site
  - export REMOTE=$(git config remote.origin.url | sed 's/.*:\/\///')
  - git remote add github https://${GH_TOKEN}@${REMOTE}
  - git add ./*
  - git status
  - git commit -m "Built by Travis (build $TRAVIS_BUILD_NUMBER)"
  - git push --quiet github master:master

notifications:
  email: false
```

具体的には、
`script:`内でビルドし`hakyll`でhtmlファイルを生成しています。
また、`after_success:`内でgithubにpushしています。  

Travis内で`stack build`するため初回は時間がかかります。
2回目以降はキャッシュされたものを使うため時間が短縮されます。

## 公開

ここまで設定しておけばpushすることで公開されます。

```
git push origin source
```

## まとめ(感想)

stackもTravisも初めてで、Haskell久しぶりな状態だったため時間がかかりましたが、
作業量はそれほどではありませんでした。心配していたcabal hellも起きませんでしたし、
チャンスがあればまたstack使ってみたいと思います。

- Stack便利
- Hakyll便利
- Travis便利
- Github Pages便利
- ブログ書くモチベーションが続けば良いな

### 参考
- (1) [Hakyllを使ってGitHub Pagesを作成して、そのソースも管理して、Travis CIで自動デプロイする](http://imokuri123.com/blog/2015/04/create-github-pages-with-hakyll.html)
- (2) [Haskellのビルドツール"stack"の紹介 - Qiita](http://qiita.com/tanakh/items/6866d0f570d0547df026)
