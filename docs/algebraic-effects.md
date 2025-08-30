# 代数的エフェクト (Algebraic Effects) ⚡

代数的エフェクトは、副作用を構造化された方法で扱う革新的なアプローチです。Lean 4 では実験的機能として、モナドベースの設計と代数的エフェクトの利点を統合しています。

## 代数的エフェクトとは？

### 1. 従来のアプローチとの比較

```lean
-- 従来のモナドスタック方式
def traditionalComputation : StateT Nat (ExceptT String IO) Unit := do
  let current ← get
  if current > 10 then
    throw "値が大きすぎます"
  else
    set (current + 1)
    liftIO (IO.println s!"現在の値: {current + 1}")

-- 代数的エフェクト風のアプローチ（Lean 4）
def algebraicComputation : EStateM String Nat Unit := do
  let current ← get
  if current > 10 then
    throw "値が大きすぎます"
  else
    set (current + 1)
    -- エフェクトが自動的に組み合わされる
```

### 2. エフェクトの分離と組み合わせ

```lean
-- エフェクトを独立して定義
def incrementCounter : EStateM ε Nat Unit := do
  modify (· + 1)

def maybeThrow (condition : Bool) : ExceptT String Id Unit := do
  if condition then
    throw "エラーが発生しました"
  else
    pure ()

-- 異なるハンドラで同じロジックを処理
def handleWithLogging : EStateM String Nat Unit := do
  incrementCounter
  let current ← get
  if current > 5 then
    throw s!"カウンターが上限を超えました: {current}"
```

## Lean 4 での代数的エフェクトの実装

### 1. EStateM - 統合状態モナド

```lean
-- EStateM は State と Except を統合
variable {ε σ : Type}

-- 基本的な使用例
def counterExample : EStateM String Nat Nat := do
  let initial ← get
  set (initial + 1)
  let current ← get
  if current > 100 then
    throw "カウンターオーバーフロー"
  else
    pure current

-- ハンドラの定義
def runCounter (computation : EStateM String Nat α) (initialState : Nat) : 
    Except String (α × Nat) :=
  computation.run initialState

-- 使用例
#eval runCounter counterExample 50    -- Except.ok (51, 51)
#eval runCounter counterExample 100   -- Except.error "カウンターオーバーフロー"
```

### 2. カスタムエフェクトの定義

```lean
-- ログエフェクトの定義
structure LogEffect (α : Type) where
  value : α
  logs : List String

instance : Monad LogEffect where
  pure a := ⟨a, []⟩
  bind la f := 
    let lb := f la.value
    ⟨lb.value, la.logs ++ lb.logs⟩

def log (msg : String) : LogEffect Unit :=
  ⟨(), [msg]⟩

-- ファイルシステムエフェクト
inductive FileSystemEffect (α : Type) where
  | readFile : String → FileSystemEffect String
  | writeFile : String → String → FileSystemEffect Unit
  | pure : α → FileSystemEffect α

instance : Monad FileSystemEffect where
  pure := FileSystemEffect.pure
  bind m f := match m with
    | FileSystemEffect.pure a => f a
    | FileSystemEffect.readFile path => 
        FileSystemEffect.readFile path >>= f  -- 簡略化
    | FileSystemEffect.writeFile path content =>
        FileSystemEffect.writeFile path content >>= f

-- エフェクトの組み合わせ
def complexComputation : LogEffect (FileSystemEffect String) := do
  log "ファイル操作を開始"
  pure (FileSystemEffect.readFile "config.txt")
```

### 3. 多重エフェクトハンドリング

```lean
-- 複数のエフェクトを持つ計算
def multiEffectComputation : EStateM String (Nat × List String) String := do
  let (counter, logs) ← get
  set (counter + 1, logs ++ ["カウンターをインクリメント"])
  
  if counter > 10 then
    throw "制限に達しました"
  else
    let (_, currentLogs) ← get
    pure s!"処理完了。ログ数: {currentLogs.length}"

-- 異なるハンドラで同じ計算を処理
def developmentHandler (comp : EStateM String (Nat × List String) α) : 
    IO (Except String (α × (Nat × List String))) := do
  let result := comp.run (0, [])
  -- 開発環境では詳細ログを出力
  match result with
  | Except.ok (value, (counter, logs)) =>
    for logEntry in logs do
      IO.println s!"[DEV] {logEntry}"
    pure (Except.ok (value, (counter, logs)))
  | Except.error e =>
    IO.println s!"[DEV ERROR] {e}"
    pure (Except.error e)

def productionHandler (comp : EStateM String (Nat × List String) α) : 
    IO (Except String α) := do
  let result := comp.run (0, [])
  -- プロダクション環境では最小限の処理
  pure (result.map (·.1))
```

## 実践的な代数的エフェクトパターン

### 1. データベースアクセス

```lean
-- データベースエフェクトの抽象化
class DatabaseEffect (m : Type → Type) where
  query : String → m (List String)
  execute : String → m Unit

-- 具体的な実装: インメモリDB
def InMemoryDB := EStateM String (List (String × String)) 

instance : DatabaseEffect InMemoryDB where
  query sql := do
    let db ← get
    -- 簡単なクエリ処理（実際はもっと複雑）
    pure (db.map (·.1))
  
  execute sql := do
    modify (· ++ [("dummy", "data")])

-- 使用例
def userService [DatabaseEffect m] : m (List String) := do
  let users ← DatabaseEffect.query "SELECT * FROM users"
  DatabaseEffect.execute "UPDATE users SET last_seen = NOW()"
  pure users

-- 異なるハンドラで同じサービスを使用
def testEnvironment := userService.run []
def realDatabase := userService  -- 実際のDB実装と組み合わせ
```

### 2. HTTP クライアント

```lean
-- HTTP エフェクトの定義
class HttpEffect (m : Type → Type) where
  get : String → m String
  post : String → String → m String

-- モックハンドラ
def MockHttp := EStateM String (List (String × String))

instance : HttpEffect MockHttp where
  get url := do
    let responses ← get
    match responses.find? (fun (u, _) => u == url) with
    | some (_, response) => pure response
    | none => throw s!"URL not found: {url}"
  
  post url body := do
    modify (· ++ [(url, s!"Posted: {body}")])
    pure "OK"

-- APIクライアント
def fetchUserData [HttpEffect m] (userId : String) : m String := do
  let userData ← HttpEffect.get s!"https://api.example.com/users/{userId}"
  HttpEffect.post "https://api.example.com/analytics" s!"accessed_user_{userId}"
  pure userData

-- テスト用のモック環境
def testApiClient : MockHttp String := do
  let mockResponses := [("https://api.example.com/users/123", "{\"name\": \"Alice\"}")]
  fetchUserData "123"

#eval testApiClient.run []
```

### 3. 非同期処理

```lean
-- 非同期エフェクト（概念的）
class AsyncEffect (m : Type → Type) where
  async : m α → m (Future α)  -- Future は仮想的な型
  await : Future α → m α

-- 並行処理の組み合わせ
def parallelProcessing [AsyncEffect m] [HttpEffect m] : m (String × String) := do
  -- 並行してAPIを呼び出し
  let future1 ← AsyncEffect.async (HttpEffect.get "https://api1.com")
  let future2 ← AsyncEffect.async (HttpEffect.get "https://api2.com")
  
  -- 両方の結果を待機
  let result1 ← AsyncEffect.await future1
  let result2 ← AsyncEffect.await future2
  
  pure (result1, result2)
```

## エフェクトの合成と変換

### 1. エフェクトトランスフォーマー

```lean
-- ReaderT風のエフェクト拡張
def withConfig (config : Config) (computation : EStateM String σ α) : 
    EStateM String (Config × σ) α := do
  let (_, state) ← get
  set (config, state)
  let result ← computation.mapState (fun (_, s) => s) (fun s => (config, s))
  pure result

-- エフェクトの変換
def liftToIO [ToString ε] (computation : EStateM ε σ α) (initialState : σ) : 
    IO (Option α) := do
  match computation.run initialState with
  | Except.ok (result, _) => pure (some result)
  | Except.error e => 
    IO.println s!"エラー: {toString e}"
    pure none
```

### 2. カスタムエフェクトハンドラ

```lean
-- リソース管理エフェクト
structure ResourceEffect (α : Type) where
  acquire : IO α
  release : α → IO Unit
  computation : α → EStateM String Unit β

def withResource (effect : ResourceEffect α) : IO (Except String β) := do
  try
    let resource ← effect.acquire
    let result := effect.computation resource |>.run ()
    effect.release resource
    pure result
  catch e =>
    pure (Except.error s!"リソースエラー: {e}")

-- 使用例
def fileProcessing : ResourceEffect (IO.Handle) := {
  acquire := IO.FS.Handle.mk "input.txt" IO.FS.Mode.read
  release := fun handle => handle.close
  computation := fun handle => do
    -- ファイル処理ロジック
    pure ()
}
```

### 3. エフェクトの動的構成

```lean
-- プラグインシステム
class Plugin (m : Type → Type) where
  name : String
  process : String → m String

-- プラグインを動的に適用
def applyPlugins [Monad m] (plugins : List (Plugin m)) (input : String) : m String := do
  let mut result := input
  for plugin in plugins do
    result ← plugin.process result
  pure result

-- 設定に基づくハンドラー選択
inductive Environment
  | development | testing | production

def selectHandler (env : Environment) : 
    (∀ α, EStateM String σ α → IO (Except String α)) :=
  match env with
  | Environment.development => developmentHandler
  | Environment.testing => testHandler
  | Environment.production => productionHandler
```

## まとめ

代数的エフェクトは、以下の利点を提供します：

1. **関心事の分離**: エフェクトの定義と処理を分離
2. **再利用性**: 同じコードを異なるハンドラで実行可能
3. **テスタビリティ**: モックハンドラによる簡単なテスト
4. **組み合わせ可能性**: 複数のエフェクトを自然に組み合わせ
5. **型安全性**: コンパイル時のエフェクト追跡

Lean 4 では、従来のモナドシステムと代数的エフェクトの利点を組み合わせることで、より表現力豊かで保守しやすいプログラムを書くことができます。これらの概念を理解し活用することで、複雑な副作用を持つプログラムを、より構造化された方法で扱えるようになります。