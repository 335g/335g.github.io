---
title: protocol oriented with 継続渡しスタイル
tag: Swift
---

### モチベーション

protocol (extension)内でenumのパターンマッチをしたい。

### 継続渡しスタイル (CPS: Continuation-passing style)

簡単な例として、入力された数字が3の倍数かどうかを判定する関数を用いて処理を分岐することを考えます。

```swift
func foo(x: Int) -> Bool {
	return x % 3 == 0
}

func main() {
	switch foo(1) {
	case true:
		print("true")
	case false:
		print("false")
	}
}
```

これを継続渡しスタイルで考えるとこんな感じになります。

```swift
func foo2<A>(x: Int, f: Bool -> A) -> A {
	return f(x % 3 == 0)
}

func main() {
	foo2(1){ x in
		switch x {
		case true:
			print("true")
		case false:
			print("false")
		}
	}
}
```

`foo(x: Int) -> Bool`の場合、

1. `foo`に値を渡し`Bool`値を得て、
2. 戻り値(`Bool`)が`true`か`false`か判断し、
3. 戻り値に応じて何かする (ここでは`print()`)

という手順になります。一方、
継続渡しスタイル`foo2<A>(x: Int, f: Bool -> A) -> A`の場合は、

1. `foo2`に値と関数を渡し、
2. `foo2`が`Bool`値を関数に適用する。

という手順になります。つまり継続渡しスタイルでは、関数の計算結果を判断するという部分を関数側に押しやってしまっているように見えます。

### 継続渡しスタイルのパターンマッチ

`enum`のパターンマッチの場合を考えます。つまり、`enum`をパターンマッチして何か行いたい場合において、パターンマッチ部分を関数側に押しやってしまうわけです。

具体例として、以下のような`Either<A, B>`を考えます。

```swift
enum Either<L, R> {
	case Left(L)
	case Right(R)
}
```

`Either<L, R>`が準拠するprotocol `EitehrType`を定義し、その中でパターンマッチ用のメソッド`either`を定義します。

```swift
protocol EitherType {
	typealias LeftType
	typealias RightType

	func either<A>(@noescape ifLeft ifLeft: LeftType -> A, @noescape ifRight: RightType -> A) -> A
}

extension Either: EitherType {
	typealias LeftType = L
	typealias RightType = R

	func either<A>(@noescape ifLeft ifLeft: L -> A, @noescape ifRight: R -> A) -> A {
		switch self {
		case .Left(let x):
			return ifLeft(x)
		case .Right(let x):
			return ifRight(x)
		}
	}
}
```

これにより`extension EitherType`内でパターンマッチが可能になります。

```swift
extension EitherType {
  var right: RightType? {
		return either(
			ifLeft: { _ in nil },
			ifRight: { $0 }
		)
	}
}

let a: Either<Int, Int> = .Left(0)
a.right // nil

let b: Either<Int, Int> = .Right(0)
b.right // 0
```

これだけだと`Either`側で実装すればいいんじゃ...と思ってしまうんですが、protocol内でパターンマッチできることで`where`文に`EitherType`が出てくる場合にもパターンマッチできるようになります。

```swift
extension Array where Element: EitherType {
	func rights() -> [Element.RightType] {
		return self
			.map{ $0.right }
			.filter{ $0 != nil }
			.map{ $0! }
	}
}

let eithers: [Either<Int, Int>] = [
  .Left(0),
  .Right(1),
  .Left(2),
  .Left(3),
  .Right(4)
]
eithers.rights() // [1, 4]
```

### 参考

- [継続渡しなHaskellライフ - モナドとわたしとコモナド](http://fumieval.hatenablog.com/entry/2014/02/11/205916)
- [robrix/Either - github](https://github.com/robrix/Either)
