# メタプログラミングの世界 (Metaprogramming in Lean) 🔮

メタプログラミングは「プログラムを書くプログラム」の技術です。Lean 4 は強力なメタプログラミング機能を提供し、コード生成、自動証明、カスタム構文の作成が可能です。

## メタプログラミングの基礎概念

### 1. マクロシステム

```lean
-- シンプルなマクロ
macro "hello" : term => `(String)

-- より複雑なマクロ：デバッグ用の出力
macro "debug!" e:term : term => `(do IO.println s!"Debug: {$e}"; pure $e)

-- 使用例
#eval debug! (2 + 3)  -- "Debug: 5" を出力してから 5 を返す

-- 条件付きコンパイルのマクロ
macro "when" cond:term "then" e:term : term =>
  `(if $cond then some $e else none)

#eval when true then "実行される"     -- some "実行される"
#eval when false then "実行されない"   -- none
```

### 2. 構文拡張

```lean
-- カスタム記法の定義
declare_syntax_cat math_expr

syntax num : math_expr
syntax math_expr "+" math_expr : math_expr
syntax math_expr "*" math_expr : math_expr
syntax "(" math_expr ")" : math_expr

-- 優先度の指定
syntax:65 math_expr "+" math_expr : math_expr
syntax:70 math_expr "*" math_expr : math_expr

-- マクロによる展開
macro "calc[" e:math_expr "]" : term => expandMathExpr e

def expandMathExpr : Syntax → MacroM Term
  | `(math_expr| $n:num) => return quote ($n : Nat)
  | `(math_expr| $a + $b) => do
    let a' ← expandMathExpr a
    let b' ← expandMathExpr b
    return quote ($a' + $b')
  | `(math_expr| $a * $b) => do
    let a' ← expandMathExpr a
    let b' ← expandMathExpr b
    return quote ($a' * $b')
  | `(math_expr| ( $e )) => expandMathExpr e
  | _ => Macro.throwUnsupported

-- 使用例
#eval calc[2 + 3 * 4]  -- 14
```

## コード生成

### 1. 構造体の自動生成

```lean
-- フィールドから getter/setter を自動生成するマクロ
macro "auto_accessors" "{" fields:ident,* "}" : command => do
  let structName := `AutoStruct
  let mut structFields := #[]
  let mut getters := #[]
  let mut setters := #[]
  
  for field in fields.getElems do
    structFields := structFields.push (quote ($field : Nat))
    getters := getters.push (quote (
      def get$(field.getId) (s : $structName) : Nat := s.$field
    ))
    setters := setters.push (quote (
      def set$(field.getId) (s : $structName) (val : Nat) : $structName := 
        { s with $field := val }
    ))
  
  let structDef := quote (structure $structName where $structFields:*)
  let result := #[structDef] ++ getters ++ setters
  
  return quote (do $result:*)

-- 使用例（概念的な例）
-- auto_accessors {x, y, z}
-- これにより AutoStruct と対応する getter/setter が生成される
```

### 2. インスタンス自動生成

```lean
-- Show インスタンスを自動生成するマクロ
macro "derive_show" structName:ident : command => do
  quote (
    instance : ToString $structName where
      toString s := s!"{$structName} \{ ... }"
  )

-- 使用例
structure Point3D where
  x : Float
  y : Float  
  z : Float

derive_show Point3D

#eval toString (Point3D.mk 1.0 2.0 3.0)
```

## タクティク（Tactics）

### 1. カスタムタクティクの作成

```lean
-- シンプルなタクティク：仮定から直接証明を見つける
syntax "find_assumption" : tactic

@[tactic find_assumption] def findAssumptionTactic : Tactic := fun stx => do
  let goal ← Tactic.getMainGoal
  let goalType ← goal.getType
  let localContext ← getLCtx
  
  for localDecl in localContext do
    if localDecl.isImplementationDetail then continue
    let type ← instantiateMVars localDecl.type
    if ← isDefEq type goalType then
      goal.assign localDecl.toExpr
      return

-- 使用例
example (h : P) : P := by find_assumption

-- より複雑なタクティク：数値計算
syntax "compute_nat" : tactic

@[tactic compute_nat] def computeNatTactic : Tactic := fun stx => do
  let goal ← Tactic.getMainGoal
  let goalType ← goal.getType
  
  -- ゴールが Nat = Nat の形か確認
  match goalType with
  | `(Nat.add $a $b = $c) => do
    -- a + b を計算して c と比較
    let result ← evalNat `(Nat.add $a $b)
    let expected ← evalNat c
    if result == expected then
      goal.assign (quote (rfl : $a + $b = $c))
  | _ => throwError "数値計算できない型です"

-- カスタムシンプリフィケーション
syntax "my_simp" : tactic

@[tactic my_simp] def mySimpTactic : Tactic := fun stx => do
  -- 基本的な簡約ルールを適用
  Tactic.evalTactic (← `(tactic| simp [Nat.add_zero, Nat.zero_add]))
```

### 2. 証明自動化

```lean
-- 自動証明タクティク：リストの性質
syntax "list_auto" : tactic

@[tactic list_auto] def listAutoTactic : Tactic := fun stx => do
  let goal ← Tactic.getMainGoal
  let goalType ← goal.getType
  
  match goalType with
  | `(List.length (List.append $xs $ys) = List.length $xs + List.length $ys) => do
    -- List.length_append を適用
    goal.assign (quote (List.length_append $xs $ys))
  | `(List.append [] $xs = $xs) => do
    goal.assign (quote (List.nil_append $xs))
  | `(List.append $xs [] = $xs) => do
    goal.assign (quote (List.append_nil $xs))
  | _ => throwError "未対応のリスト性質です"

-- 使用例
example (xs ys : List Nat) : List.length (List.append xs ys) = List.length xs + List.length ys := 
  by list_auto
```

## Template Haskell スタイルのメタプログラミング

### 1. コードの内省と変換

```lean
-- 関数の自動memoization
def memoize (f : α → β) : α → β := 
  -- 実際の実装は複雑になりますが、概念的な例
  f  -- 簡略化

macro "auto_memo" f:ident : command => do
  quote (def $(f.getId)Memo := memoize $f)

-- フィボナッチ数列
def fib : Nat → Nat
  | 0 => 0
  | 1 => 1  
  | n + 2 => fib n + fib (n + 1)

auto_memo fib  -- fibMemo を生成

-- パフォーマンステスト用のマクロ
macro "benchmark" e:term : term => 
  quote (do
    let start ← IO.monoMsNow
    let result ← pure $e
    let stop ← IO.monoMsNow
    IO.println s!"実行時間: {stop - start}ms"
    pure result
  )

-- 使用例
#eval benchmark (fib 30)
```

### 2. DSL (Domain Specific Language) の構築

```lean
-- 状態機械のDSL
declare_syntax_cat state_machine
declare_syntax_cat state_def
declare_syntax_cat transition

syntax ident : state_def
syntax state_def "→" state_def : transition
syntax "states" "{" state_def,* "}" "transitions" "{" transition,* "}" : state_machine

-- SQLライクなクエリDSL
declare_syntax_cat query
declare_syntax_cat table_name
declare_syntax_cat field_name
declare_syntax_cat condition

syntax ident : table_name
syntax ident : field_name
syntax field_name "=" term : condition

syntax "SELECT" "*" "FROM" table_name "WHERE" condition : query

macro "sql!" q:query : term => do
  match q with
  | `(query| SELECT * FROM $table WHERE $field = $value) =>
    quote (queryTable $(quote table.getId) $(quote field.getId) $value)
  | _ => Macro.throwUnsupported

-- 使用例（概念的）
-- #eval sql! SELECT * FROM users WHERE age = 25
```

### 3. 型レベルプログラミング

```lean
-- 型レベルでの計算
inductive TypeLevel where
  | zero : TypeLevel
  | succ : TypeLevel → TypeLevel

-- 型レベル加法
type family Add : TypeLevel → TypeLevel → TypeLevel where
  Add zero n => n
  Add (succ m) n => succ (Add m n)

-- 長さが型で保証されたベクトル（再掲）
inductive Vec (α : Type) : Nat → Type where
  | nil : Vec α 0
  | cons : α → Vec α n → Vec α (n + 1)

-- 型安全な配列アクセス
inductive Fin : Nat → Type where
  | zero : Fin (n + 1)
  | succ : Fin n → Fin (n + 1)

def Vec.get : Vec α n → Fin n → α
  | Vec.cons x _, Fin.zero => x
  | Vec.cons _ xs, Fin.succ i => Vec.get xs i

-- マクロによる配列リテラルの生成
macro "vec![" elems:term,* "]" : term => do
  let elements := elems.getElems
  let mut result := quote (Vec.nil)
  
  for elem in elements.reverse do
    result := quote (Vec.cons $elem $result)
  
  return result

-- 使用例
#eval vec![1, 2, 3, 4]  -- Vec.cons 1 (Vec.cons 2 (Vec.cons 3 (Vec.cons 4 Vec.nil)))
```

## 実践的なメタプログラミング例

### 1. JSON シリアライゼーションの自動生成

```lean
-- 構造体から JSON シリアライザーを自動生成
macro "derive_json" structName:ident : command => do
  -- 構造体の情報を取得（実際の実装は複雑）
  let toJsonImpl := quote (
    instance : ToJSON $structName where
      toJSON s := 
        -- フィールドを反復してJSONオブジェクトを構築
        "{}"  -- 簡略化
  )
  
  let fromJsonImpl := quote (
    instance : FromJSON $structName where
      fromJSON json := 
        -- JSONからフィールドをパースして構造体を構築
        some default  -- 簡略化
  )
  
  return quote (do
    $toJsonImpl
    $fromJsonImpl
  )

-- 使用例
structure User where
  id : Nat
  name : String
  email : String

derive_json User
```

### 2. テストケースの自動生成

```lean
-- プロパティベーステストのマクロ
macro "quickcheck" prop:term : command => do
  quote (
    #eval do
      let tests := 100
      for i in [0:tests] do
        -- ランダムな入力を生成
        let input ← generateRandom
        if not ($prop input) then
          IO.println s!"テスト失敗: {input}"
          break
        else
          continue
      IO.println "全てのテストが成功しました"
  )

-- 例：ソート関数のテスト
def isSorted (xs : List Nat) : Bool :=
  match xs with
  | [] => true
  | [_] => true
  | x :: y :: rest => x ≤ y && isSorted (y :: rest)

-- quickcheck (fun xs => isSorted (List.quicksort xs))
```

### 3. 設定駆動コード生成

```lean
-- 設定ファイルからAPIクライアントを生成
structure APIEndpoint where
  name : String
  method : String
  path : String
  params : List (String × String)

def endpoints : List APIEndpoint := [
  { name := "getUser", method := "GET", path := "/users/{id}", params := [("id", "Nat")] },
  { name := "createUser", method := "POST", path := "/users", params := [("user", "User")] }
]

-- エンドポイントから関数を生成するマクロ
macro "generate_api_client" endpoints:term : command => do
  -- endpoints をパースして各エンドポイント用の関数を生成
  quote (
    namespace APIClient
      -- 生成される関数群
      def getUser (id : Nat) : IO (Option User) := sorry
      def createUser (user : User) : IO Bool := sorry
    end APIClient
  )

-- generate_api_client endpoints
```

## メタプログラミングのベストプラクティス

### 1. エラーハンドリング

```lean
-- 適切なエラーメッセージを提供
macro "safe_div" a:term b:term : term => do
  match b with
  | `(0) => Macro.throwErrorAt b "ゼロ除算は許可されていません"
  | _ => return quote ($a / $b)

-- 型チェック付きマクロ
macro "typed_assert" e:term ":" t:term : term => do
  -- 式 e の型が t と一致するかチェック
  return quote (($e : $t))
```

### 2. デバッグ支援

```lean
-- マクロ展開のトレース
macro "trace_expand" e:term : term => do
  trace[Meta.debug] "マクロ展開: {e}"
  return e

-- コンパイル時情報の提供
macro "compile_time_info" : term => do
  let time := getCurrentTime  -- 概念的
  return quote (s!"コンパイル時刻: {time}")
```

### 3. パフォーマンス最適化

```lean
-- コンパイル時定数畳み込み
macro "const_fold" e:term : term => do
  -- 式 e が定数計算可能かチェック
  if isConstant e then
    let result ← evaluateAtCompileTime e
    return quote ($result)
  else
    return e

-- インライン展開の制御
macro "force_inline" f:ident : command => do
  quote (@[inline] def $f := $f)
```

メタプログラミングにより、Lean の表現力は飛躍的に向上します。型安全性を保ちながら、反復的なコードの自動生成、DSL の構築、証明の自動化が可能になります。これらの技術を駆使して、より効率的で表現豊かなプログラムを作成してみてください！