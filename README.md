# Learn Lean Language

Lean言語の学習用リポジトリです。

## 使用方法

### Dockerイメージのビルド
```bash
make build
```

### Leanファイルの実行
```bash
make exec DIR=directory FILE=filename.lean
```

例：
```bash
make exec DIR=algorithms FILE=eratosthenes.lean
make exec DIR=basics FILE=sample.lean
```

### その他のコマンド
- `make run` - Leanコンテナを対話モードで起動
- `make clean` - Dockerイメージを削除
- `make help` - 利用可能なコマンドを表示

## ディレクトリ構成

```
learn-lean-language/
├── src/
│   ├── basics/
│   │   └── sample.lean          # 基本的なサンプルコード
│   └── algorithms/
│       └── eratosthenes.lean    # エラトステネスの篩の実装
├── tests/                       # テストファイル
├── docs/                        # ドキュメント
├── Dockerfile
├── Makefile
└── README.md
```
