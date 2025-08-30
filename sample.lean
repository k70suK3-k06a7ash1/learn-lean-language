-- Sample Lean program
def hello : String := "Hello, Lean!"

def add (a b : Nat) : Nat := a + b

def factorial : Nat → Nat
  | 0 => 1
  | n + 1 => (n + 1) * factorial n

#eval hello
#eval add 3 5
#eval factorial 5

-- Proof example
theorem add_comm (a b : Nat) : a + b = b + a := by
  rw [Nat.add_comm]

#check add_comm