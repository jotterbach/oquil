open Complex
open Core

module Pauli : sig
  type pauli =
    | N
    | I of t * int
    | SX of t * int
    | SY of t * int
    | SZ of t * int
    | P of pauli list
    | S of pauli list

  val ( + ) : pauli -> pauli -> pauli
  val ( $* ): t -> pauli -> pauli
  val ( <|> ): pauli -> pauli -> pauli
end = struct
  type pauli =
    | N
    | I of t * int
    | SX of t * int
    | SY of t * int
    | SZ of t * int
    | P of pauli list
    | S of pauli list

  let ( + ) a b = match (a, b) with
    | (S x, S y) -> S (x @ y)
    | (S x, y) -> S (x @ [y])
    | (x, S y) -> S ([x] @ y)
    | (x, y) -> S [x; y]

  let rec ( $* ) f p = match p with
    | I (t, i) -> I (mul t f, i)
    | SZ (w, i) -> SZ (mul f w, i)
    | SY (w, i) -> SY (mul f w, i)
    | SX (w, i) -> SX (mul f w, i)
    | P [] -> P []
    | P [x] -> f $* x
    | P (hd :: tl) -> P ( [(f $* hd)] @ tl )
    | S [] -> S []
    | S [x] -> f $* x
    | S (hd :: tl) -> (f $* hd) + ( f $* (S tl))

  let rec ( <|> ) a b = match (a, b) with
    | (N, _) -> N
    | (I _, x) -> x
    | (_, N ) -> N
    | (x, I _) -> x
    | (SX (f, i), SX (t, j)) -> N
    | (SY (f, i), SY (t, j)) -> N
    | (SZ (f, i), SZ (t, j)) -> N
    | (SX (f, i), SY (t, j)) -> if i=j then Complex.i $* ( SZ (mul f t, i) ) else N
    | (SY (f, i), SX (t, j)) -> if i=j then Complex.(conj i) $* ( SZ (mul f t, i) ) else N
    | (SY (f, i), SZ (t, j)) -> if i=j then Complex.i $* ( SX (mul f t, i) ) else N
    | (SZ (f, i), SY (t, j)) -> if i=j then Complex.(conj i) $* ( SX (mul f t, i) ) else N
    | (SZ (f, i), SX (t, j)) -> if i=j then Complex.i $* ( SY (mul f t, i) ) else N
    | (SX (f, i), SZ (t, j)) -> if i=j then Complex.(conj i) $* ( SY (mul f t, i) ) else N
    | (SX (f, i), S []) -> N
    | (SX (f, i), S [x]) -> SX (f, i) <|> x
    | (SX (f, i), S (hd :: tl)) -> (SX (f, i) <|> hd) + ((SX (f, i)) <|> (S tl))

end
