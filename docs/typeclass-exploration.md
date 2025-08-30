# 型クラスの探求 (Type Class Exploration) 🔍

型クラスは Lean の最も強力な抽象化メカニズムの一つです。Haskell の型クラスに似ており、異なる型に対して共通のインターfaces を提供します。

## 基本概念

### 型クラスとは？
型クラスは「型の型」- 特定の操作や性質を持つ型の集合を定義します。

```lean
-- 基本的な型クラスの例
class Printable (α : Type) where
  toString : α → String

instance : Printable Nat where
  toString n := s!"{n}"

instance : Printable Bool where
  toString b := if b then "true" else "false"
```

## 標準型クラスの深掘り

### 1. Functor - 構造を保ったマッピング

```lean
-- Functor は map 操作を提供
class Functor (f : Type → Type) where
  map : (α → β) → f α → f β

-- Option に対する Functor インスタンス
instance : Functor Option where
  map f
    | none => none
    | some a => some (f a)

-- List に対する Functor インスタンス  
instance : Functor List where
  map := List.map

-- 使用例
#eval Functor.map (· + 1) [1, 2, 3]  -- [2, 3, 4]
#eval Functor.map (· * 2) (some 5)   -- some 10
```

### 2. Applicative - 複数の引数を持つ関数の適用

```lean
class Applicative (f : Type → Type) extends Functor f where
  pure : α → f α
  seq : f (α → β) → f α → f β

-- Option での実装
instance : Applicative Option where
  pure := some
  seq
    | some f, some a => some (f a)
    | _, _ => none

-- 使用例: 複数の Option 値を組み合わせる
def addThree (a b c : Option Nat) : Option Nat :=
  pure (· + · + ·) <*> a <*> b <*> c

#eval addThree (some 1) (some 2) (some 3)  -- some 6
#eval addThree (some 1) none (some 3)      -- none
```

### 3. Monad - 順序付きコンピュテーション

```lean
-- 既に見たように、Monad は bind と pure を提供
class Monad (m : Type → Type) extends Applicative m where
  bind : m α → (α → m β) → m β

-- カスタムモナド: Writer モナド（ログ付きコンピュテーション）
structure Writer (w α : Type) where
  value : α
  log : w

instance [Monoid w] : Monad (Writer w) where
  pure a := ⟨a, Monoid.neutral⟩
  bind wa f :=
    let wb := f wa.value
    ⟨wb.value, wa.log ++ wb.log⟩

def tell (w : Type) [Monoid w] (msg : w) : Writer w Unit :=
  ⟨(), msg⟩

-- 使用例
def computation : Writer String Nat := do
  tell "Starting computation\n"
  let x := 5
  tell s!"x = {x}\n" 
  let y := x * 2
  tell s!"y = x * 2 = {y}\n"
  pure y

#eval computation  -- ⟨10, "Starting computation\nx = 5\ny = x * 2 = 10\n"⟩
```

## 高度な型クラス設計

### 1. 型クラスの継承階層

```lean
-- 抽象代数構造の階層
class Semigroup (α : Type) where
  op : α → α → α

class Monoid (α : Type) extends Semigroup α where
  neutral : α

class Group (α : Type) extends Monoid α where
  inv : α → α

-- 実装例: 整数の加法群
instance : Semigroup Int where
  op := (· + ·)

instance : Monoid Int where
  neutral := 0

instance : Group Int where
  inv := (- ·)

-- 群の性質を利用した関数
def power [Group α] (x : α) : Nat → α
  | 0 => Monoid.neutral
  | n + 1 => Semigroup.op x (power x n)
```

### 2. 型レベルでの計算

```lean
-- 型レベルでの自然数
inductive TypeNat where
  | zero : TypeNat
  | succ : TypeNat → TypeNat

-- 型レベルでのベクトル（長さが型に含まれる）
inductive Vec (α : Type) : TypeNat → Type where
  | nil : Vec α TypeNat.zero
  | cons : α → Vec α n → Vec α (TypeNat.succ n)

-- 長さが保証された append
def Vec.append : Vec α n → Vec α m → Vec α (TypeNat.add n m)
  | Vec.nil, ys => ys
  | Vec.cons x xs, ys => Vec.cons x (xs.append ys)
```

### 3. Dependent Type Classes

```lean
-- インデックス付き型クラス
class DecidableEq (α : Type) where
  decEq : (a b : α) → Decidable (a = b)

-- 使用例: 安全なリスト検索
def safeFind [DecidableEq α] (p : α → Bool) (xs : List α) : Option α :=
  xs.find? p

-- カスタム型での実装
inductive Color where
  | red | green | blue

instance : DecidableEq Color where
  decEq
    | Color.red, Color.red => isTrue rfl
    | Color.green, Color.green => isTrue rfl  
    | Color.blue, Color.blue => isTrue rfl
    | Color.red, Color.green => isFalse nofun
    | Color.red, Color.blue => isFalse nofun
    | Color.green, Color.red => isFalse nofun
    | Color.green, Color.blue => isFalse nofun
    | Color.blue, Color.red => isFalse nofun
    | Color.blue, Color.green => isFalse nofun
```

## 実践的な型クラス設計

### 1. Serialization 型クラス

```lean
class ToJSON (α : Type) where
  toJSON : α → String

class FromJSON (α : Type) where  
  fromJSON : String → Option α

instance : ToJSON Nat where
  toJSON n := toString n

instance : ToJSON String where
  toJSON s := s!"\"{{s}}\""

instance [ToJSON α] : ToJSON (List α) where
  toJSON xs := s!"[{String.intercalate \",\" (xs.map ToJSON.toJSON)}]"

-- 自動導出可能な構造体での使用
structure Person where
  name : String
  age : Nat

instance : ToJSON Person where
  toJSON p := s!"\{\"name\":{ToJSON.toJSON p.name},\"age\":{ToJSON.toJSON p.age}}"

#eval ToJSON.toJSON (Person.mk "Alice" 30)
-- "{\"name\":\"Alice\",\"age\":30}"
```

### 2. Show と Debug 型クラス

```lean
class Show (α : Type) where
  show : α → String

class Debug (α : Type) where
  debug : α → String

-- 異なる表現を提供
instance : Show Person where
  show p := s!"{p.name} (age {p.age})"

instance : Debug Person where  
  debug p := s!"Person \{ name := \"{p.name}\", age := {p.age} }"

def alice := Person.mk "Alice" 30
#eval Show.show alice   -- "Alice (age 30)"
#eval Debug.debug alice -- "Person { name := \"Alice\", age := 30 }"
```

## 型クラスの合成とリファクタリング

### 1. 型クラス制約の組み合わせ

```lean
-- 複数の型クラス制約を要求する関数
def processAndShow [ToJSON α] [Show α] [Debug α] (x : α) : String :=
  s!"JSON: {ToJSON.toJSON x}\nShow: {Show.show x}\nDebug: {Debug.debug x}"

-- 型クラス束（Type Class Bundle）
class Displayable (α : Type) extends ToJSON α, Show α, Debug α

-- 自動的に全ての制約を満たす
def processDisplayable [Displayable α] (x : α) : String :=
  processAndShow x
```

### 2. 条件付きインスタンス

```lean
-- Maybe型（Optionに似た）の定義
inductive Maybe (α : Type) where
  | nothing : Maybe α
  | just : α → Maybe α

-- 内部型がShowなら、MaybeもShow
instance [Show α] : Show (Maybe α) where
  show
    | Maybe.nothing => "Nothing"
    | Maybe.just x => s!"Just ({Show.show x})"

-- 内部型がEqなら、MaybeもEq  
instance [BEq α] : BEq (Maybe α) where
  beq
    | Maybe.nothing, Maybe.nothing => true
    | Maybe.just x, Maybe.just y => x == y
    | _, _ => false
```

この型クラス探求により、Leanの型システムの表現力と、抽象化による code reusability の力を実感できるでしょう。次は代数的データ型の探求に移りましょう！