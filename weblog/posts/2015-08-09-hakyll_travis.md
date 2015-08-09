---
title: Hakyll × stack × Travis CI × Github でブログを書く
---

やった事をまとめると、以下の通り。

- StackでHaskellのパッケージを管理する。
- markdownでブログを書き、Hakyllでhtmlファイルに変換する。
- 実際には、TravisがHakyllを使ってコンパイルしhtmlファイルに変換する。
- ソースファイルはGithubの公開リポジトリで管理し、Github Pagesで公開する。

できたのが[これ](https://github.com/335g/335g.github.io)。

## [Stack](https://github.com/commercialhaskell/stack)

Haskellのパッケージをビルドしたりインストールしたりするツールです。
[@tanakh](https://twitter.com/tanakh)さんによる
[紹介記事](http://qiita.com/tanakh/items/6866d0f570d0547df026)をきっかけに知りました。
Haskell力のない自分にはcabal hellを乗り越えられる自信が無かったという理由と、
タイムリーなので使ってみたいという理由から選択してみました。

## [Hakyll](http://jaspervdj.be/hakyll/)

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
具体的には`site.hs`を見ながら[チュートリアル](http://jaspervdj.be/hakyll/tutorials.html)を読んでみてください。シンプルな構成になっているのでなんとなくであればすぐに理解できると思います。(自分もまだ理解不足ですが)

## [Travis CI](https://travis-ci.org)

CIを行うためのサービス。
[Stackを用いるための設定方法](https://github.com/commercialhaskell/stack/wiki/Travis)が公開されています。

## [Github Pages](https://pages.github.com)

Githubのユーザやリポジトリ毎に提供されるWebページ公開サービス。ソースはGithubのリポジトリで管理する。

## 記事作成 〜 公開 の流れ

- `source`: markdownで記事を書く
- `source`: hakyllでコンパイルし正しく表示されるか確認する (省略可)
- `source`: githubにpushする
- `master`: Travisがコンパイルしhtmlファイルを生成＆githubにpushする (自動)
- `master`: Github Pagesに公開される

つまり、記事生成を`source`ブランチで行ってgithubにpushすれば、
`master`ブランチに必要なファイルが作成され公開されるという事です。

## 準備

### stackをインストール

[releases](https://github.com/commercialhaskell/stack/releases)から最新版をダウンロード。
パスが通っているディレクトリに配置する。

### ビルドする

```
cabal init
```

まず`.cabal`ファイルを作っておきます。
注意するところとしては、ライセンスをきちんと設定しておかないと
(ディレクトリに`LILCENSE`が無いと)後にこけてしまいます。  

次に、生成された`.cabal`ファイルに`hakyll`依存を明記します。

```
build-depends: hakyll >=4.5
```

Stackageで管理されている安定verが4.6.9.0 (2015.8.9時点)なので、
それが満たせるような表記になっていれば問題無いかと思います。

```
stack init
```

これで`stack.yml`が生成されます。  

次に`stack build`をします。すると現時点のバージョンでは、
`hfsevents: needed (>=0.1.3)`と怒られてしまいます。ログの下の方に

```
Recommended action: try adding the following to your extra-deps in .../stack.yaml
- hfsevents-0.1.5
```
とあるので、`stack.yml`の`extra-deps`の項に追記します。

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

インストール終了後に

```
stack exec hakyll-init weblog
```

とすることで、`weblog`フォルダに必要ファイル一式が生成されます。
ここまででビルドできるようになるのですが、まずはgit submodule設定を行ってしまいます。

### Travisの設定をする

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
urlを`https://github.com/<account name>/<account name>.github.io.git`に修正します。  

必要無いファイルは管理し無いように`.gitignore`を作成します。

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

`.travis.yml`はこんな感じです。

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
作業量はそれほどではありませんでした。心配していたcabal hellも起きませんし、
チャンスがあればまたstack使ってみたいと思います。ただ、その前にcssなんとかしないとw

- Stack便利
- Hakyll便利
- Travis便利
- Github Pages便利
- ブログ書くモチベーションが続けば良いな

### 参考
- (1) [Hakyllを使ってGitHub Pagesを作成して、そのソースも管理して、Travis CIで自動デプロイする](http://imokuri123.com/blog/2015/04/create-github-pages-with-hakyll.html)
- (2) [Haskellのビルドツール"stack"の紹介 - Qiita](http://qiita.com/tanakh/items/6866d0f570d0547df026)