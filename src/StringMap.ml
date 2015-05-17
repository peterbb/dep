
module StringMap = Map.Make(String)

include StringMap

let dom m = 
    let add k _ s = StringSet.add k s in
    StringMap.fold add m StringSet.empty

