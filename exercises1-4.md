# GADT Exercises: Levels 1–4

-----

## Level 1: Type-Indexed Expression Evaluator

### Goal

Build a small typed expression language where `eval` is total — no `option`, no exceptions, no impossible cases.

### Starter types

```ocaml
type _ expr =
  | Int  : int -> int expr
  | Bool : bool -> bool expr
  | Add  : int expr * int expr -> int expr
```

### Tasks

1. **Write `eval : type a. a expr -> a`** for the starter type above. Verify that the return type is `a`, not a tagged union.
1. **Add these constructors and extend `eval`:**
- `If : bool expr * 'a expr * 'a expr -> 'a expr` — both branches must agree on type
- `Lt : int expr * int expr -> bool expr`
- `And : bool expr * bool expr -> bool expr`
- `Not : bool expr -> bool expr`
1. **Add `Eq : 'a expr * 'a expr -> bool expr`** where both sides have the same (but arbitrary) type. Think about what this means for the evaluator — which equality function can you use?
1. **Write `to_string : type a. a expr -> string`** that pretty-prints the expression with parentheses. Example: `Add (Int 1, Int 2)` becomes `"(1 + 2)"`.
1. **Write `optimize : type a. a expr -> a expr`** that performs constant folding and simplification:
- `Add (Int 0, e)` → `e`
- `And (Bool true, e)` → `e`
- `Not (Bool b)` → `Bool (not b)`
- `If (Bool true, t, _)` → `t`
- Bonus: make it recursive so nested simplifications work.

### Test cases

```ocaml
let () =
  (* eval *)
  assert (eval (Int 42) = 42);
  assert (eval (Add (Int 1, Int 2)) = 3);
  assert (eval (Bool true) = true);
  assert (eval (If (Lt (Int 1, Int 2), Int 10, Int 20)) = 10);
  assert (eval (And (Bool true, Not (Bool false))) = true);
  assert (eval (Eq (Int 3, Add (Int 1, Int 2))) = true);

  (* to_string *)
  assert (to_string (Add (Int 1, Int 2)) = "(1 + 2)");
  assert (to_string (If (Bool true, Int 1, Int 2)) = "(if true then 1 else 2)");

  (* optimize *)
  assert (optimize (Add (Int 0, Int 5)) = Int 5);
  assert (optimize (If (Bool true, Int 1, Int 2)) = Int 1);
  assert (optimize (Not (Bool false)) = Bool true)
```

### What you should learn

- The GADT constructor syntax with explicit return types.
- Locally abstract type annotations (`type a.`).
- How pattern matching refines the type index per branch.
- How existentially quantified type variables work in `Eq`.

-----

## Level 2: Type-Safe Printf

### Goal

Build a format descriptor that statically determines the type of a formatting function. `sprintf (FInt ^^ lit " is " ^^ FBool)` should have type `int -> bool -> string`.

### Key insight

The challenge is that the *type of the function* depends on the *value of the format*. A format with two `FInt` holes needs a function `int -> int -> string`. The GADT index encodes this as a nested function type.

### Starter types

```ocaml
type (_, _) fmt =
  | Lit    : string -> ('a, 'a) fmt
  | FInt   : ('a, int -> 'a) fmt
  | FString : ('a, string -> 'a) fmt
  | FBool  : ('a, bool -> 'a) fmt
  | Cat    : ('b, 'c) fmt * ('a, 'b) fmt -> ('a, 'c) fmt
```

The two type parameters are a CPS trick:

- The first (`'a`) is the “final result” type (will be `string`).
- The second (`'c`) is the type of the function you need to call.
- `Lit` doesn’t consume any arguments, so both parameters are the same.
- `FInt` says “I need one more `int` argument.”
- `Cat` composes two formats by chaining their continuations.

### Tasks

1. **Write `sprintf : type a. (string, a) fmt -> a`.**
   Hint: you’ll need a helper that accumulates a `Buffer.t` or string list.
   The actual signature of the internal function will be:

   ```ocaml
   val go : type a b. Buffer.t -> (a, b) fmt -> (unit -> a) -> b
   ```

   where the continuation receives the final accumulated string.
   Then `sprintf fmt = go (Buffer.create 16) fmt Fun.id`.
1. **Define a convenient concatenation operator:**

   ```ocaml
   let ( ^^ ) a b = Cat (a, b)
   ```
1. **Verify these work:**

   ```ocaml
   let () =
     assert (sprintf (lit "hello") = "hello");
     assert (sprintf (FInt ^^ lit " + " ^^ FInt ^^ lit " = " ^^ FInt) 1 2 3
             = "1 + 2 = 3");
     assert (sprintf (lit "flag: " ^^ FBool) true = "flag: true");
     assert (sprintf (FString ^^ lit " is " ^^ FInt ^^ lit " years old")
               "Alice" 30
             = "Alice is 30 years old")
   ```
1. **Add `FFloat : ('a, float -> 'a) fmt`** and make it print with two decimal places.
1. **(Stretch) Add `FPad : int -> ('a, string -> 'a) fmt`** that left-pads a string to a given width. Note that the width is a *value* baked into the format descriptor, not a runtime argument.

### What you should learn

- CPS-encoded type indices — the two-parameter GADT trick.
- How `Cat` composes type-level function signatures.
- Polymorphic recursion where both type parameters change at each step.
- This is essentially how OCaml’s `Format` and `Printf` modules work internally.

-----

## Level 3: Phantom-Indexed State Machine

### Goal

Model a file handle protocol where the compiler rejects invalid operation sequences. You should not be able to read from a closed handle or close an already-closed handle.

### Starter types

```ocaml
type closed
type open_r  (* open for reading *)
type open_w  (* open for writing *)

type _ handle
(* Abstract — the implementation is up to you. Could wrap a Unix.file_descr,
   or just be a dummy for the exercise. *)
```

### Tasks

1. **Define the operations with phase-constrained signatures:**

   ```ocaml
   val open_read  : string -> open_r handle
   val open_write : string -> open_w handle
   val read_line  : open_r handle -> string * open_r handle
   val write_line : open_w handle -> string -> open_w handle
   val close      : _ handle -> closed handle
   ```

   Note: `close` accepts any open handle but returns a `closed handle`.
   Note: `read_line` and `write_line` return a new handle — this is the
   linear-style trick to prevent using a stale handle after close.
1. **Implement a mock version** using an internal `string list ref` or similar, where `read_line` pops from the list and `write_line` pushes.
1. **Write a valid pipeline and verify it compiles:**

   ```ocaml
   let () =
     let h = open_read "test.txt" in
     let (line, h) = read_line h in
     let (line2, h) = read_line h in
     let _ = close h in
     print_endline line;
     print_endline line2
   ```
1. **Verify these are compile errors (comment them out after testing):**

   ```ocaml
   (* Should fail: reading from a write handle *)
   let _ = let h = open_write "out.txt" in read_line h

   (* Should fail: writing to a read handle *)
   let _ = let h = open_read "in.txt" in write_line h "hello"

   (* Should fail: reading after close *)
   let _ =
     let h = open_read "in.txt" in
     let h = close h in
     read_line h
   ```
1. **(Stretch) Add a read-write mode:**

   ```ocaml
   type open_rw
   val open_readwrite : string -> open_rw handle
   ```

   The challenge: `read_line` should accept both `open_r` and `open_rw`,
   and `write_line` should accept both `open_w` and `open_rw`. You have
   a few options:
- Use a GADT with capability witnesses
- Use polymorphic variants as phantom indices
- Use a “has read capability” / “has write capability” encoding

   Try at least two approaches and compare the ergonomics.

### What you should learn

- Typestate encoding in OCaml.
- How returning a new handle simulates linear usage (even without linear types).
- The limits of this approach — OCaml can’t prevent you from using `h` after
  passing it to `close`, since there’s no ownership. The pattern is conventional,
  not enforced.
- The design space for encoding capability sets in the type system.

-----

## Level 4: Length-Indexed Vectors

### Goal

Build vectors (fixed-length lists) where the length is tracked in the type. `head` should be total (no option), and `zip` should only accept vectors of equal length.

### Starter types

```ocaml
type zero
type _ succ

type (_, _) vec =
  | Nil  : ('a, zero) vec
  | Cons : 'a * ('a, 'n) vec -> ('a, 'n succ) vec
```

### Tasks

1. **Write these basic operations:**

   ```ocaml
   val head : ('a, _ succ) vec -> 'a
   val tail : ('a, _ succ) vec -> (* what's the return type? *)
   val length : ('a, 'n) vec -> int
   ```

   Note that `head` and `tail` don’t return `option` — the type rules out
   the empty case.
1. **Write `map`:**

   ```ocaml
   val map : ('a -> 'b) -> ('a, 'n) vec -> ('b, 'n) vec
   ```

   The length index is preserved.
1. **Write `zip`:**

   ```ocaml
   val zip : ('a, 'n) vec -> ('b, 'n) vec -> ('a * 'b, 'n) vec
   ```

   Both inputs must have the same length `'n`. Verify that
   `zip (Cons (1, Nil)) (Cons ("a", Cons ("b", Nil)))` is a type error.
1. **Write `append`:**

   ```ocaml
   (* You'll need type-level addition. *)
   type (_, _, _) add =
     | AddZ : (zero, 'n, 'n) add
     | AddS : ('m, 'n, 'p) add -> ('m succ, 'n, 'p succ) add

   val append : ('m, 'n, 'p) add -> ('a, 'm) vec -> ('a, 'n) vec -> ('a, 'p) vec
   ```

   The `add` witness is a proof that `m + n = p`. The caller must provide it.
   This is clunky compared to Lean where the kernel would compute the
   addition — that’s the key ergonomic gap.
1. **Write `reverse`:**
   This is harder than it looks. You need the length to be preserved, but
   the naive accumulator approach changes the type at each step. You’ll
   likely need `append` or a similar trick.

   ```ocaml
   val reverse : ('a, 'n) vec -> ('a, 'n) vec
   ```

   Hint: one approach is an existential accumulator with an `add` witness
   that you build up as you go. Another is to use `append` with a
   singleton. Neither is pretty — and that’s the point.
1. **Write `to_list` and `of_list`:**

   ```ocaml
   val to_list : ('a, _) vec -> 'a list

   (* of_list must hide the length existentially *)
   type 'a some_vec = Vec : ('a, _) vec -> 'a some_vec
   val of_list : 'a list -> 'a some_vec
   ```

   Note how `of_list` loses the length information — the length depends on
   the runtime value, so it must be existentially wrapped.

### Test cases

```ocaml
let v1 = Cons (1, Cons (2, Cons (3, Nil)))
let v2 = Cons (10, Cons (20, Cons (30, Nil)))
let v3 = Cons ("a", Cons ("b", Nil))

let () =
  assert (head v1 = 1);
  assert (length v1 = 3);
  assert (to_list (map (fun x -> x * 2) v1) = [2; 4; 6]);
  assert (to_list (zip v1 v2) = [(1,10); (2,20); (3,30)]);

  (* This should be a type error — uncomment to verify: *)
  (* let _ = zip v1 v3 in () *)

  let witness = AddS (AddS (AddS AddZ)) in
  assert (to_list (append witness v1 v2) = [1; 2; 3; 10; 20; 30]);

  assert (to_list (reverse v1) = [3; 2; 1])
```

### What you should learn

- Type-level natural numbers in OCaml.
- Polymorphic recursion where the index decrements.
- Existential wrapping when the index is runtime-dependent.
- Why `append` and `reverse` need arithmetic witnesses, and how painful
  that is compared to dependently-typed languages — this builds real
  appreciation for what Lean gives you for free.
- The practical limits of encoding numeric invariants in OCaml’s type system.

-----

## General Tips

- **When the typechecker gives a confusing error**, the first thing to check
  is whether you’re missing a `type a.` (or `type a n.`) annotation.
- **Existential types can’t escape their scope.** If you match on a GADT
  constructor that introduces an existential, you can’t return it outside
  the match branch unpackaged.
- **Use utop for experimentation.** GADTs are much easier to develop
  interactively.
- **Read the error messages carefully.** OCaml’s GADT errors often say
  exactly what went wrong — “this type would escape its scope” means you
  tried to leak an existential.
