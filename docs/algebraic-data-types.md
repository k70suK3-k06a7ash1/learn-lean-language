# 代数的データ型で遊ぶ (Algebraic Data Types Playground) 🌳

代数的データ型は関数型プログラミングの核心です。Lean では強力な型システムを使って、複雑なデータ構造を安全かつ表現豊かに定義できます。

## 基本的な代数的データ型

### 1. Sum Types (直和型) - 「AまたはB」

```lean
-- シンプルな列挙型
inductive Direction where
  | north | south | east | west

-- より複雑な sum type
inductive Result (α β : Type) where
  | success : α → Result α β
  | error : β → Result α β

-- パターンマッチング
def handleResult [ToString α] [ToString β] (r : Result α β) : String :=
  match r with
  | Result.success val => s!"成功: {toString val}"
  | Result.error err => s!"エラー: {toString err}"

#eval handleResult (Result.success 42)
#eval handleResult (Result.error "ファイルが見つかりません")
```

### 2. Product Types (直積型) - 「AかつB」

```lean
-- レコード構文
structure Point where
  x : Float
  y : Float

-- タプル的な表現
inductive Pair (α β : Type) where
  | mk : α → β → Pair α β

-- 複雑な構造体
structure Person where
  name : String
  age : Nat
  address : String
  hobbies : List String

def alice : Person := {
  name := "Alice"
  age := 30
  address := "東京"
  hobbies := ["読書", "プログラミング", "料理"]
}
```

## 再帰的データ構造

### 1. 自然数の定義

```lean
-- Peano自然数
inductive MyNat where
  | zero : MyNat
  | succ : MyNat → MyNat

-- 加法の定義
def MyNat.add : MyNat → MyNat → MyNat
  | a, MyNat.zero => a
  | a, MyNat.succ b => MyNat.succ (MyNat.add a b)

-- 乗法の定義
def MyNat.mul : MyNat → MyNat → MyNat
  | _, MyNat.zero => MyNat.zero
  | a, MyNat.succ b => MyNat.add a (MyNat.mul a b)

-- 便利な記法
instance : Add MyNat := ⟨MyNat.add⟩
instance : Mul MyNat := ⟨MyNat.mul⟩

def three : MyNat := MyNat.succ (MyNat.succ (MyNat.succ MyNat.zero))
def two : MyNat := MyNat.succ (MyNat.succ MyNat.zero)

#eval three + two   -- 実際には計算されません（表示の問題）
```

### 2. リスト構造の探求

```lean
-- 独自のList定義
inductive MyList (α : Type) where
  | nil : MyList α
  | cons : α → MyList α → MyList α

-- 便利な記法
infixr:67 " :: " => MyList.cons

-- 基本操作
def MyList.length : MyList α → Nat
  | MyList.nil => 0
  | MyList.cons _ tail => 1 + MyList.length tail

def MyList.append : MyList α → MyList α → MyList α
  | MyList.nil, ys => ys
  | MyList.cons x xs, ys => MyList.cons x (MyList.append xs ys)

def MyList.map (f : α → β) : MyList α → MyList β
  | MyList.nil => MyList.nil
  | MyList.cons x xs => MyList.cons (f x) (MyList.map f xs)

-- fold操作
def MyList.foldl (f : β → α → β) (init : β) : MyList α → β
  | MyList.nil => init
  | MyList.cons x xs => MyList.foldl f (f init x) xs

-- 使用例
def numbers : MyList Nat := 1 :: 2 :: 3 :: MyList.nil
#eval MyList.length numbers                    -- 3
#eval MyList.foldl (· + ·) 0 numbers          -- 6
```

## 二分木とその操作

### 1. 基本的な二分木

```lean
-- 二分木の定義
inductive BTree (α : Type) where
  | leaf : BTree α
  | node : α → BTree α → BTree α → BTree α

-- ヘルパー関数
def BTree.singleton (x : α) : BTree α :=
  BTree.node x BTree.leaf BTree.leaf

-- 木の操作
def BTree.size : BTree α → Nat
  | BTree.leaf => 0
  | BTree.node _ left right => 1 + BTree.size left + BTree.size right

def BTree.height : BTree α → Nat
  | BTree.leaf => 0
  | BTree.node _ left right => 1 + max (BTree.height left) (BTree.height right)

def BTree.mirror : BTree α → BTree α
  | BTree.leaf => BTree.leaf
  | BTree.node x left right => BTree.node x (BTree.mirror right) (BTree.mirror left)

-- トラバーサル
def BTree.inorder : BTree α → List α
  | BTree.leaf => []
  | BTree.node x left right => 
    BTree.inorder left ++ [x] ++ BTree.inorder right

def BTree.preorder : BTree α → List α
  | BTree.leaf => []
  | BTree.node x left right => 
    [x] ++ BTree.preorder left ++ BTree.preorder right
```

### 2. 二分探索木 (BST)

```lean
-- 順序関係を利用したBST
def BTree.insert [Ord α] (x : α) : BTree α → BTree α
  | BTree.leaf => BTree.singleton x
  | BTree.node y left right =>
    match compare x y with
    | Ordering.lt => BTree.node y (BTree.insert x left) right
    | Ordering.eq => BTree.node y left right  -- 重複を許可しない
    | Ordering.gt => BTree.node y left (BTree.insert x right)

def BTree.contains [Ord α] (x : α) : BTree α → Bool
  | BTree.leaf => false
  | BTree.node y left right =>
    match compare x y with
    | Ordering.lt => BTree.contains x left
    | Ordering.eq => true
    | Ordering.gt => BTree.contains x right

-- リストからBSTを構築
def BTree.fromList [Ord α] (xs : List α) : BTree α :=
  xs.foldl (fun tree x => BTree.insert x tree) BTree.leaf

-- 使用例
def sampleTree := BTree.fromList [5, 3, 8, 1, 4, 7, 9]
#eval BTree.inorder sampleTree    -- ソートされた順序: [1, 3, 4, 5, 7, 8, 9]
```

## Rose Tree (多分木)

```lean
-- 任意の数の子を持つ木
inductive Rose (α : Type) where
  | node : α → List (Rose α) → Rose α

-- Rose Tree の操作
def Rose.size : Rose α → Nat
  | Rose.node _ children => 
    1 + children.foldl (fun acc child => acc + Rose.size child) 0

def Rose.height : Rose α → Nat
  | Rose.node _ children =>
    match children.map Rose.height with
    | [] => 1
    | heights => 1 + heights.foldl max 0

def Rose.map (f : α → β) : Rose α → Rose β
  | Rose.node x children => Rose.node (f x) (children.map (Rose.map f))

-- ファイルシステムのモデリング例
inductive FileSystem where
  | file : String → Nat → FileSystem      -- name, size
  | dir : String → List FileSystem → FileSystem

def FileSystem.totalSize : FileSystem → Nat
  | FileSystem.file _ size => size
  | FileSystem.dir _ contents => contents.foldl (fun acc fs => acc + FileSystem.totalSize fs) 0

def sampleFS : FileSystem :=
  FileSystem.dir "home" [
    FileSystem.dir "user" [
      FileSystem.file "readme.txt" 100,
      FileSystem.file "config.json" 250,
      FileSystem.dir "projects" [
        FileSystem.file "main.lean" 500,
        FileSystem.file "test.lean" 200
      ]
    ]
  ]

#eval FileSystem.totalSize sampleFS  -- 1050
```

## グラフ構造

### 1. 隣接リスト表現

```lean
-- 有向グラフ
structure DirectedGraph (α : Type) where
  vertices : List α
  edges : List (α × α)

-- グラフの操作
def DirectedGraph.neighbors [BEq α] (graph : DirectedGraph α) (vertex : α) : List α :=
  graph.edges.filterMap fun (from, to) =>
    if from == vertex then some to else none

def DirectedGraph.hasEdge [BEq α] (graph : DirectedGraph α) (from to : α) : Bool :=
  graph.edges.any fun (a, b) => a == from && b == to

-- DFS (深さ優先探索)
def DirectedGraph.dfsHelper [BEq α] (graph : DirectedGraph α) (visited : List α) (current : α) : List α :=
  if visited.contains current then visited
  else
    let newVisited := current :: visited
    let neighbors := graph.neighbors current
    neighbors.foldl (fun acc neighbor => graph.dfsHelper acc neighbor) newVisited

def DirectedGraph.dfs [BEq α] (graph : DirectedGraph α) (start : α) : List α :=
  (graph.dfsHelper [] start).reverse
```

### 2. 木としてのグラフ（Forest）

```lean
-- 森（複数の木の集合）
inductive Forest (α : Type) where
  | empty : Forest α
  | tree : α → Forest α → Forest α → Forest α  -- 値、子、兄弟

def Forest.size : Forest α → Nat
  | Forest.empty => 0
  | Forest.tree _ children siblings => 1 + Forest.size children + Forest.size siblings

-- 森から平坦なリストへ
def Forest.flatten : Forest α → List α
  | Forest.empty => []
  | Forest.tree x children siblings => 
    [x] ++ Forest.flatten children ++ Forest.flatten siblings
```

## パーサーコンビネーター用のデータ型

### 1. 式（Expression）の AST

```lean
-- 算術式の抽象構文木
inductive Expr where
  | num : Int → Expr
  | add : Expr → Expr → Expr
  | mul : Expr → Expr → Expr
  | sub : Expr → Expr → Expr
  | div : Expr → Expr → Expr
  | var : String → Expr

-- 式の評価
partial def Expr.eval (env : String → Int) : Expr → Int
  | Expr.num n => n
  | Expr.add a b => Expr.eval env a + Expr.eval env b
  | Expr.mul a b => Expr.eval env a * Expr.eval env b
  | Expr.sub a b => Expr.eval env a - Expr.eval env b
  | Expr.div a b => Expr.eval env a / Expr.eval env b
  | Expr.var name => env name

-- 式の文字列表現
def Expr.toString : Expr → String
  | Expr.num n => s!"{n}"
  | Expr.add a b => s!"({Expr.toString a} + {Expr.toString b})"
  | Expr.mul a b => s!"({Expr.toString a} * {Expr.toString b})"
  | Expr.sub a b => s!"({Expr.toString a} - {Expr.toString b})"
  | Expr.div a b => s!"({Expr.toString a} / {Expr.toString b})"
  | Expr.var name => name

-- 使用例
def sampleExpr : Expr := 
  Expr.add (Expr.mul (Expr.var "x") (Expr.num 2)) (Expr.num 5)

def env (name : String) : Int :=
  if name == "x" then 10 else 0

#eval Expr.eval env sampleExpr     -- 25
#eval Expr.toString sampleExpr     -- "(x * 2) + 5"
```

### 2. 型付きλ計算

```lean
-- 単純型
inductive SimpleType where
  | int : SimpleType
  | bool : SimpleType
  | arrow : SimpleType → SimpleType → SimpleType

-- λ計算の項
inductive Term where
  | var : String → Term
  | app : Term → Term → Term
  | lam : String → SimpleType → Term → Term
  | intLit : Int → Term
  | boolLit : Bool → Term

-- 型環境
def TypeEnv := String → Option SimpleType

-- 型チェック
def Term.typeCheck (env : TypeEnv) : Term → Option SimpleType
  | Term.var name => env name
  | Term.intLit _ => some SimpleType.int
  | Term.boolLit _ => some SimpleType.bool
  | Term.app f arg => do
    let fType ← Term.typeCheck env f
    let argType ← Term.typeCheck env arg
    match fType with
    | SimpleType.arrow from to => 
      if from == argType then some to else none
    | _ => none
  | Term.lam param paramType body =>
    let newEnv := fun name => if name == param then some paramType else env name
    let bodyType ← Term.typeCheck newEnv body
    some (SimpleType.arrow paramType bodyType)
```

## 実践例: JSON パーサー

```lean
-- JSON値の型
inductive JsonValue where
  | null : JsonValue
  | bool : Bool → JsonValue
  | number : Float → JsonValue
  | string : String → JsonValue
  | array : List JsonValue → JsonValue
  | object : List (String × JsonValue) → JsonValue

-- JSON の pretty printing
def JsonValue.toString : JsonValue → String
  | JsonValue.null => "null"
  | JsonValue.bool true => "true"
  | JsonValue.bool false => "false"
  | JsonValue.number n => s!"{n}"
  | JsonValue.string s => s!"\"{s}\""
  | JsonValue.array arr => 
    s!"[{String.intercalate \", \" (arr.map JsonValue.toString)}]"
  | JsonValue.object obj =>
    let pairs := obj.map fun (k, v) => s!"\"{k}\": {JsonValue.toString v}"
    s!"{{String.intercalate \", \" pairs}}"

-- 使用例
def sampleJson : JsonValue := 
  JsonValue.object [
    ("name", JsonValue.string "Alice"),
    ("age", JsonValue.number 30),
    ("hobbies", JsonValue.array [
      JsonValue.string "読書",
      JsonValue.string "プログラミング"
    ])
  ]

#eval JsonValue.toString sampleJson
```

代数的データ型により、複雑なデータ構造を型安全に表現し、パターンマッチングによる強力な操作が可能になります。これらの例を基に、さらに複雑なデータ構造や domain-specific な型を設計してみてください！