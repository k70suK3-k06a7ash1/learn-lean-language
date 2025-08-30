-- Monadic Pattern Samples in Lean - Using Monad Type Class

-- Generic monadic operations using the Monad type class

-- Generic function that works with any monad
def applyTwice [Monad m] (f : α → m α) (x : α) : m α := do
  let y ← f x
  f y

-- Generic lifting of binary operations
def liftM2 [Monad m] (f : α → β → γ) (ma : m α) (mb : m β) : m γ := do
  let a ← ma
  let b ← mb
  pure (f a b)

-- Example 1: Using Monad class with Option
def safeDiv (a b : Nat) : Option Nat :=
  if b = 0 then none else some (a / b)

-- Using generic monadic bind (>>=) directly
def chainedDivision : Option Nat :=
  safeDiv 100 5 >>= fun x =>
  safeDiv x 4 >>= fun y =>
  safeDiv y 2

-- Using do notation (syntactic sugar for bind)
def chainedDivisionDo : Option Nat := do
  let x ← safeDiv 100 5
  let y ← safeDiv x 4
  safeDiv y 2

-- Using generic applyTwice with Option
def doubleSafeDiv (n : Nat) : Option Nat := safeDiv n 2

#eval applyTwice doubleSafeDiv 16    -- some 4

-- Example 2: Using Monad class with Except
inductive MyError
  | DivisionByZero
  | NegativeInput

def safeDivExcept (a b : Int) : Except MyError Int :=
  if a < 0 then Except.error MyError.NegativeInput
  else if b = 0 then Except.error MyError.DivisionByZero
  else Except.ok (a / b)

-- Using generic liftM2 with Except
def addResults : Except MyError Int :=
  liftM2 (· + ·) (safeDivExcept 10 2) (safeDivExcept 20 4)

-- Example 3: Custom Monad implementation

-- Custom Monad - Logger with proper Monad instance
structure Logger (α : Type) where
  value : α
  log : String

instance : Monad Logger where
  pure a := ⟨a, ""⟩
  bind la f := 
    let lb := f la.value
    ⟨lb.value, la.log ++ lb.log⟩

def loggedAdd (a b : Nat) : Logger Nat :=
  ⟨a + b, s!"Added {a} + {b} = {a + b}\n"⟩

def loggedMul (a b : Nat) : Logger Nat :=
  ⟨a * b, s!"Multiplied {a} * {b} = {a * b}\n"⟩

-- Using generic monadic operations with Logger
def complexCalculation : Logger Nat := do
  let sum ← loggedAdd 3 4
  let product ← loggedMul sum 2
  pure product

-- Using our generic liftM2 with Logger
def loggedBinaryOp : Logger Nat :=
  liftM2 (· + ·) (loggedAdd 2 3) (loggedMul 4 5)

#eval complexCalculation
#eval loggedBinaryOp

-- Example 4: Generic monadic sequence operations
-- sequenceM works with any monad
def processWithMonad [Monad m] (f : Nat → m Nat) (xs : List Nat) : m (List Nat) :=
  xs.mapM f

-- Using with Option
def maybeDouble (x : Nat) : Option Nat :=
  if x % 2 = 0 then some (x * 2) else none

#eval processWithMonad maybeDouble [2, 4, 6]    -- some [4, 8, 12]
#eval processWithMonad maybeDouble [2, 3, 6]    -- none

-- Using with Logger
def loggedDouble (x : Nat) : Logger Nat :=
  ⟨x * 2, s!"Doubled {x} to get {x * 2}\n"⟩

#eval processWithMonad loggedDouble [1, 2, 3]

-- Example 5: Generic monadic composition
def kleisliCompose [Monad m] (f : α → m β) (g : β → m γ) : α → m γ :=
  fun x => f x >>= g

-- Example with Option
def halveDivTwo (n : Nat) : Option Nat := safeDiv n 2
def divByFour (n : Nat) : Option Nat := safeDiv n 4

def composedOperation := kleisliCompose halveDivTwo divByFour

#eval composedOperation 32    -- some 4

-- Example 6: Monad laws demonstration (generic version)
-- These work with any monad, not just Option
variable [Monad m] [LawfulMonad m]

def leftIdentity (a : α) (f : α → m β) : Prop :=
  (pure a >>= f) = f a

def rightIdentity (ma : m α) : Prop :=
  (ma >>= pure) = ma

def associativity (ma : m α) (f : α → m β) (g : β → m γ) : Prop :=
  ((ma >>= f) >>= g) = (ma >>= fun x => f x >>= g)

-- Example 7: Using Monad class methods directly
-- filterM - filters using a monadic predicate
def isEvenM (n : Nat) : Option Bool :=
  if n % 2 = 0 then some true else some false

def filterEvenNumbers (xs : List Nat) : Option (List Nat) :=
  xs.filterM isEvenM

#eval filterEvenNumbers [1, 2, 3, 4, 5, 6]    -- some [2, 4, 6]