open GNatural

val () =
    Unit.checkExpectWith (Unit.listString Unit.intString) 
    "number conversion 2017"
    (fn () => decimal (ofInt 2017))
    [2, 0, 1, 7]

val () =
    Unit.checkExpectWith (Unit.recordString [("quotient", Unit.intString), ("remainder", Unit.intString)]) 
    "sdiv 84536 by 1"
    (fn () => (ofInt 84536) sdiv 1)
    { quotient = ofInt 84536, remainder = 0 }

