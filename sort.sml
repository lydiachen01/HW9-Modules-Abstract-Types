functor PQSortFn(structure Q : PQUEUE) :> SORT where type elem = Q.elem =
    struct
        type elem = Q.elem

        fun compare (elem1, elem2) = Q.compare_elem(elem1, elem2)

        (* fun to sort a list using a PQ *)
        fun sort xs =
            let
                val pq = foldl Q.insert Q.empty xs

                (* extract elems from the PQ in sorted order and accumulate
                them into a list *)
                fun deleteInsert (pq, acc) =
                    if Q.isEmpty pq then 
                        List.rev acc (* return the acc list in rev order *)
                    else
                        let
                            val (e, q) = Q.deletemin pq (* remove min elem *)
                        in
                            (* recurse with the updated queue and the elem
                            added to the acc *)
                            deleteInsert (q, e :: acc)
                        end
            in 
                deleteInsert (pq, [])
            end
    end
