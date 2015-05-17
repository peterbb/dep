
module String_map = Map.Make(String)

include String_map

let dom m = 
    let add k _ s = String_set.add k s in
    String_map.fold add m String_set.empty

