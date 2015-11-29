---
title: Functors in Swift
tag: Swift
---

最近[Prism](https://hackage.haskell.org/package/Chart-1.1/docs/Control-Lens-Prism.html)の勉強をしています。
勉強といってもHaskell力がさほどないのでコードとにらめっこしている時間が長いのですが。
その途中で[面白い記事](https://www.fpcomplete.com/user/liyang/profunctors)を見つけました。
Haskellで出てくる様々なFunctorを紹介した記事です。今回はこれらをSwiftで定義してみようという記事です。  

1. [Covariant Functors](#covariant-functors)
1. [ContraVariant Functors](#contravariant-functors)
1. [Bifunctors](#bifunctors)
1. [Profunctors](#profunctors)

### Covariant Functors

通常のFunctorのことです。Haskellでは以下のように表記します。

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

`f a`という型が`fmap`により`f b`という型になリます。
Swiftっぽく説明するため図解するとこんな感じでしょうか。

```
             f
    A +--------------> B
  X<A>               X<B>
```

`A`というジェネリックパラメータを持つ型`X<A>`が関数`f(x: A) -> B`とともに
新たな型`X<B>`へとマップされます。Swiftでは様々な`Protocol`で`map`が実装されているため
馴染み深いと思います。Haskellで`Functor`クラスとして定義しているのに対し、
Swiftでは様々な`Protocol`で`map`が実装されています。
これはジェネリックパラメータが異なる型を`Protocol`で表現できないためです。

```Swift
// × Functor<T>という表記ができない (Swift2.1現在)

protcol Functor<T> {
  func map<U>(f: T -> U) -> Functor<U>
}
```

そのため、`CollectionType`の`map`がリストを返さざるをえないのはなんとも残念な感じです。

```swift
// ex. CollectionType
func map<T>(@noescape transform: (Self.Base.Generator.Element) -> T) -> [T]
```

ただし、抽象化してしまうと`default implementation`が機能しなくなるため、
`default implementation`を使うという方針が変わらない限りこのような抽象化はされないような
気がします。

### Contravariant Functors

次に`Contravariant`クラス。

```Haskell
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
```

まず図解してみます。

```
             f
    B +--------------> A
  X<A>               X<B>
```

先ほどとの違いは、mapする時の関数`f`における引数と返り値の型が逆になっていることです。
これを満足できるような型としては、ジェネリックパラメータの型を引数にとるような関数を
持っている場合が挙げられます。具体例を書くとこんな感じ。

```Swift
struct Predicate<T> {
	let getPredicate: T -> Bool

	func contramap<U>(g: U -> T) -> Predicate<U> {
		return Predicate<U>(getPredicate: { self.getPredicate(g($0)) })
	}
}

let odd: Predicate<Int> = Predicate(getPredicate: { $0 % 2 != 0 })
let str: Predicate<String> = odd.contramap{ $0.characters.count }
```

### Bifunctors

次に`Bifunctor`クラス。

```Hasekll
class Bifunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
```

`Bifunctor`クラスは2つの型引数を取るクラスです。
(3つ取るのを`trifunctors`と表記するらしい)
まずはこれも図解してみます。

```
               f
      A +--------------> B
        |              |
        |              |
X<A, C> |              | X<B, D>
        |              |
        |              |
      C +--------------> D
               g
```

`f :: a -> b`と`g :: c -> d`を引数に取り、`f a c`から`f b d`にマップします。
Swiftっぽい表記をするならば、`f<A, B>(a: A) -> B`と`g<C, D>(c: C) -> D`の
2つの関数により、`X<A, C>`から`X<B, D>`にマップすると言うことになります。  

Swiftで2つのtype parameterをセットにとるものとして`Either<T, U>`がよく用いられます。

```Swift
enum Either<A, C> {
	case Left(A)
	case Right(C)

	func bimap<B, D>(f: A -> B)(_ g: C -> D) -> Either<B, D> {
		switch self {
		case .Left(let x):
			return .Left(f(x))
		case .Right(let x):
			return .Right(g(x))
		}
	}
}

let left: Either<String, String> = Either.Left("0")
let right: Either<String, String> = Either.Right("0.0")
let f: String -> Int = { Int($0)! + 1 }
let g: String -> Float = { Float($0)! + 2.0 }

left.bimap(f)(g)  // Either<Int, Float>.Left(1)
right.bimap(f)(g) // Either<Int, Float>.Right(2.0)
```

### Profunctors

最後に`Profunctor`です。

```Haskell
class Profunctor f where
  dimap :: (a -> b) -> (c -> d) -> f b c -> f a d
```

```
               f
      B <--------------+ A
        |              |
        |              |
X<B, C> |              | X<A, D>
        |              |
        |              |
      C +--------------> D
               g
```

`Profunctor`クラスも2つの型引数を取るクラスです。
`Bifunctor`の図と見比べてもらえばわかるように、上の矢印の向きが逆になっています。
`Covariant`に対する`Contravariant`のような感じですね。  

Haskellにおける具体例としては`(->)`が挙げられています。
`(->)`は関数の中置き表示でして、`A->B->C->D`という風にぐるっと回ってこれるというものです。
詳しくは参照ページにて解説してもらうとして、Swiftではどう書けるのか。

どのようなサンプルが説明に適しているのかわからなかったのですが、以下のように`dimap`が定義可能です。

```swift
struct Indexed<I, A, B> {
	let runIndexed: I -> A -> B

	func dimap<C, D>(f: C -> A)(_ g: B -> D) -> Indexed<I, C, D> {
		return Indexed<I, C, D>(runIndexed: { index in
			return { g(self.runIndexed(index)(f($0))) }
		})
	}
}
```

ここで定義した`Indexed<I, A, B>`が関数`dimap(f)(g)`により
`Indexed<I, C, D>`にマップされます。

応用事例などはこれから勉強します。

### 参考
- [I love profunctors. They're so easy. - FPComplete](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors)
- [ファンクタであそぼう - 非現実的 非日常的 非常識的、非日記。](http://kinokkory.hatenablog.com/entry/20131203/p1)
