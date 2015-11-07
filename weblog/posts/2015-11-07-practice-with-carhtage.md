---
title: Carthageでμframeworkをお試しする
tags: Carthage
---

githubで面白そうなframeworkを見つけたとします。ドキュメントがあればまずはそれを読むでしょう。それで使い方や特徴が全て理解できれば良いのですが、なかなかそうもいかないところ。最近ではplaygroundでドキュメントを書いてくれているものもあり、わかりやすくはなっているのですがそれでもやはり自分で手を動かさないとわかりづらいというものです。  

iOS界隈のframework管理といえばCocoaPodsだったのですが、最近ではCarthageが盛んです。このCarthageを使うと手軽にお試しできるので手順を記しておきます。簡単で説明することが無いですね(笑)  

### 手順

- Cartfile を用意する
- `carthage update`
- workspace を作成する
- `Carhage/Checkouts` 内にある xcodeproj を workspace で読み込む
- workspace 内に playground 作ってお試しする

以上で playground 内で読み込んで使うことができます。
