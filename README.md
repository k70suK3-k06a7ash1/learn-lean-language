# Learn Lean Language

Lean言語の学習用リポジトリです。

## 使用方法

### Dockerイメージのビルド
```bash
make build
```

### Leanファイルの実行
```bash
make exec FILE=filename.lean
```

例：
```bash
make exec FILE=eratosthenes.lean
make exec FILE=sample.lean
```

### その他のコマンド
- `make run` - Leanコンテナを対話モードで起動
- `make clean` - Dockerイメージを削除
- `make help` - 利用可能なコマンドを表示

## サンプルファイル

- `eratosthenes.lean` - エラトステネスの篩の実装
- `sample.lean` - その他のサンプルコード
