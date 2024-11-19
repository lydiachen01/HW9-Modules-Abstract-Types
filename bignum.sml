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
                    | SIGN of sign * N.nat

    exception BadDivision
    exception LeftAsExercise

    fun ofInt 0 = ZERO
      | ofInt x = 
        let
            val cond = N.compare(x, 0)
        in
            if cond = GREATER then SIGN (POS, x)
            else SIGN (NEG, ~x)
        end
    
    fun negated x = ZERO <-> x
    
    fun n <+> m     = N./+/ (n, m)
    fun n <-> m     = N./-/ (n, m)
    fun n <*> m     = N./*/ (n, m)

    fun compare (bigint x, bigint y) =
        (case (bigint x, bigint y) of
                (ZERO, ZERO)  => EQUAL
            |   (ZERO, _)     => LESS
            |   (_, ZERO)     => GREATER
            |   (a, b)        => 
                    if a /</ b then LESS
                    else if a />/ b then GREATER
                    else EQUAL)

        
    (* Contract for "Short division" sdiv:

    sdiv (n, d) nonnegative bigint n by nonnegative machine integer d 
    using short division. The return value is a record of the form
      
    { quotient = q, remainder = r}

    where q and r satisfy these laws:

    n == q /*/ ofInt d /+/ ofInt r
    0 <= r < d

    Given a negative n or d <= 0, sdiv (n, d) raises BadDivision.
    *)
    val sdiv    = raise LeftAsExercise 


  fun _ sdiv 0 = raise BadDivisor
    | ZERO sdiv _ = { quotient = ZERO, remainder = 0 }
    | bigint x sdiv 1 = { quotient = bigint x, remainder = 0 }
    | N.timesBasePlus(bigint m, v) sdiv d =
        if d < 0 orelse d > base then raise BadDivisor
        else
          let
            val { quotient = q, remainder = r } = bigint m sdiv d
            val q' = N.timesBasePlus (bigint q, (bigint r * base + v) div d)
            val r' = (bigint r * base + v) mod d
          in
            { quotient = q', remainder = r' }
          end









    (* toString n returns a string giving the natural 
    representation of n in the decimal system.  If n is
    negative, toString should use the conventional minus sign "-".

    And when toString returns a string containing two or more digits,
    the first digit must not be zero.
    *)
    val toString = raise LeftAsExercise 

end
