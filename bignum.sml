(* Define three data constructors: one each for positive numbers, negative
numbers, and zero. A data constructor for a positive or negative number is
applied to a magnitude. The data constructor for zero is a big integer all by
itself. *)


functor BignumFn(structure N : NATURAL) :> BIGNUM =
struct
(* A bigint represented with a sign (either positive or negative) and a
* magnitude (a natural).
  *
  * Invariants on our representation:
  
  * In values of form POS * nat
    * - represents only positive integers, 0 (0+) included
    * - nat must follow the nat invariant
    
  * In values of form NEG * nat:
    * - represents only negative integers, 0 (-0) included
    * - sdiv will raise an error when called on this invariant
    * - nat must follow the nat invariant
*)

    datatype sign = POS 
                  | NEG
    datatype bigint = ZERO
                    | BIGINT of sign * N.nat

    exception BadDivision
    exception LeftAsExercise
      
    val /+/ = N./+/
    val /-/ = N./-/
    val /*/ = N./*/
    infix 6 /+/ /-/ 
    infix 7 /*/

    infix 6 <+> <-> 
    infix 7 <*> sdiv

    fun ofInt x = 
        case N.compare(N.ofInt x, N.ofInt 0) of
            GREATER => BIGINT(POS, N.ofInt x)
            | LESS => 
              let 
                val addOne = x + 1
                val negate = ~addOne
                val toNat = N.ofInt negate
                val newNat = N./+/ (toNat, N.ofInt 1)
              in 
                BIGINT (NEG, newNat)
              end
            | EQUAL => ZERO
    
    fun negated ZERO = ZERO
      | negated (BIGINT (sign, m)) = 
        (case sign of
            POS => BIGINT(NEG, m)
          | NEG => BIGINT(POS, m))

    fun ZERO <+> b = b
      | a <+> ZERO = a
      | (BIGINT(s1, n)) <+> (BIGINT(s2, m)) = 
          if s1 = s2 then BIGINT(s1, n /+/ m)
          else case N.compare(n, m) of
                    GREATER   => BIGINT(s1, n /-/ m)
                  | LESS      => BIGINT(s2, m /-/ n)
                  | _         => ZERO

    fun ZERO <-> b = negated b
      | a <-> ZERO = a
      | (BIGINT(s1, n)) <-> (BIGINT(s2, m)) = 
          let
            val s3 = if (s2 = POS) then NEG else POS
          in
            (BIGINT (s1, n)) <+> (BIGINT(s3, m))
          end

    fun ZERO <*> _ = ZERO
      | _ <*> ZERO = ZERO
      | (BIGINT(s1, n)) <*> (BIGINT(s2, m)) = 
          if s1 = s2 then BIGINT(POS, n /*/ m)
          else BIGINT(NEG, n /*/ m)
    
    fun compare (ZERO, ZERO) = EQUAL
    | compare (ZERO, BIGINT(POS, _)) = LESS
    | compare (ZERO, BIGINT(NEG, _)) = GREATER
    | compare (BIGINT(POS, _), ZERO) = GREATER
    | compare (BIGINT(NEG, _), ZERO) = LESS
    | compare (BIGINT(s1, n), BIGINT(s2, m)) = 
          case (s1, s2) of 
            (POS, POS) => N.compare (n, m)
          | (NEG, NEG) => N.compare (m, n)
          | (POS, NEG) => GREATER
          | (NEG, POS) => LESS


    (* Contract for "Short division" sdiv:

    sdiv (n, d) nonnegative bigint n by nonnegative machine integer d 
    using short division. The return value is a record of the form
      
    { quotient = q, remainder = r}

    where q and r satisfy these laws:

    n == q /*/ ofInt d /+/ ofInt r
    0 <= r < d

    Given a negative n or d <= 0, sdiv (n, d) raises BadDivision.
    *)

    fun _ sdiv 0 = raise BadDivision
      | ZERO sdiv _ = { quotient = ZERO, remainder = 0 }
      | (BIGINT (s1, n)) sdiv d = 
          if (s1 = NEG) orelse (d < 0) then raise BadDivision
          else 
              let val { quotient = q, remainder = r} =  N.sdiv (n,d)
              in { quotient = BIGINT (s1, q), remainder = r }
          end


    (* toString n returns a string giving the natural 
    representation of n in the decimal system.  If n is
    negative, toString should use the conventional minus sign "-".

    And when toString returns a string containing two or more digits,
    the first digit must not be zero.
    *)

    fun toString ZERO = "0"
      | toString (BIGINT(s, m)) = 
          let
            val nStr = foldr (fn (x, acc) => Int.toString x ^ acc) "" 
              (N.decimal m)
            val addSign = if s = NEG then "-" ^ nStr else nStr
          in
            addSign
          end
end
