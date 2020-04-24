# Changelog

# 0.1.0
## 新機能など
- 入力ライブラリのリファクタリング [#2](https://github.com/jueve/compro/issues/1)
    - 入力の関数のうち、いくつかをリファクタリングした
    - `String`型と`ByteString`型のリストを生成する関数を追加した
    - `String`型の入力を変更
- GHC拡張の追加 [#4](https://github.com/jueve/compro/issues/4)

## バグ修正など
- ダイクストラ法のコードを修正 [#1](https://github.com/jueve/compro/issues/2)
- ライセンス表記の修正[#7](https://github.com/jueve/compro/issues/7)


# 0.0.4
Int, Double型の読み込みを変更
- `app/template/Template.hs`の`int`
- `app/template/Template.hs`の`double`

# 0.0.3
素数判定関数のバグを修正
`app/lib/IsPrime.hs`の`isPrime`

# 0.0.2
ByteStringの入力バグを修正
`app/template/Template.hs`の`strBS`

# 0.0.1
最初のリリース