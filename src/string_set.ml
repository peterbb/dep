
module String_set = Set.Make(String)
include String_set

let union_list xs = List.fold_left String_set.union String_set.empty xs
