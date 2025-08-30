/-- `n`以下の素数のリストを `Array Bool` の形で返す。
`i` 番目が `true` ならば `i` は素数で、`false` ならば合成数。 -/
def eratosthenesAux (n : Nat) : Array Bool := Id.run do
  let mut isPrime := Array.replicate (n + 1) true

  isPrime := isPrime.set! 0 false
  isPrime := isPrime.set! 1 false

  for p in [2 : n + 1] do
    if not isPrime[p]! then
      continue

    if p ^ 2 > n then
      break

    -- `p` の倍数を消していく
    let mut q := p * p
    while q ≤ n do
      isPrime := isPrime.set! q false
      q := q + p

  return isPrime

/-- エラトステネスの篩 -/
def eratosthenes (n : Nat) : Array Nat :=
  eratosthenesAux n
  |>.zipIdx
  |>.filterMap fun ⟨isPrime, i⟩ =>
    if isPrime then some i else none

#guard eratosthenes 10 = #[2, 3, 5, 7]

#guard
  let actual := eratosthenes 100
  let expected := #[
    2, 3, 5, 7, 11,
    13, 17, 19, 23, 29,
    31, 37, 41, 43, 47,
    53, 59, 61, 67, 71,
    73, 79, 83, 89, 97
  ]
  expected == actual

#eval do
  IO.println s!"10以下の素数: {eratosthenes 10}"
  IO.println s!"30以下の素数: {eratosthenes 30}"
  IO.println s!"100以下の素数: {eratosthenes 100}"
